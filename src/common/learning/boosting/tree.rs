//! A regression tree as a flat arena of nodes, grown leaf-wise by always splitting the leaf whose best split gains
//! the most.

use std::num::NonZeroUsize;

use crate::common::learning::boosting::Penalty;
use crate::common::learning::boosting::bins::{Bins, Codes, MISSING};
use crate::common::learning::boosting::histogram::{Histogram, Statistics};
use crate::common::learning::loss::Score;
use crate::common::monoid::{Fold, Monoid, concatenate};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Node {
    /// A value at or below `threshold` goes left, every value does when it is `None`, and a missing one follows
    /// `missing_left`.
    Split {
        input: usize,
        threshold: Option<f64>,
        missing_left: bool,
        gain: f64,
        left: usize,
        right: usize,
    },
    Leaf {
        value: Score,
    },
}

/// Nodes with the root first and each child after its parent, so every walk from the root ends at a leaf.
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(try_from = "Vec<Node>", into = "Vec<Node>")]
pub struct Tree {
    nodes: Vec<Node>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum TreeError {
    #[error("a tree has at least one node")]
    Empty,
    #[error("node {node} points at node {child}, which is not after it in a tree of {length}")]
    Child {
        node: usize,
        child: usize,
        length: usize,
    },
}

impl TryFrom<Vec<Node>> for Tree {
    type Error = TreeError;

    fn try_from(nodes: Vec<Node>) -> Result<Self, Self::Error> {
        if nodes.is_empty() {
            return Err(TreeError::Empty);
        }
        for (node, entry) in nodes.iter().enumerate() {
            match entry {
                Node::Split { left, right, .. } => {
                    for &child in [left, right] {
                        if child <= node || child >= nodes.len() {
                            return Err(TreeError::Child {
                                node,
                                child,
                                length: nodes.len(),
                            });
                        }
                    }
                }
                Node::Leaf { .. } => {}
            }
        }
        Ok(Self { nodes })
    }
}

impl From<Tree> for Vec<Node> {
    fn from(tree: Tree) -> Self {
        tree.nodes
    }
}

impl Tree {
    pub fn leaf(value: Score) -> Self {
        Self {
            nodes: vec![Node::Leaf { value }],
        }
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn score(&self, row: &[f64]) -> Score {
        let mut index = 0;
        loop {
            match self.nodes[index] {
                Node::Leaf { value } => return value,
                Node::Split {
                    input,
                    threshold,
                    missing_left,
                    left,
                    right,
                    ..
                } => {
                    let value = row[input];
                    let goes_left = if value.is_nan() {
                        missing_left
                    } else {
                        threshold.is_none_or(|threshold| value <= threshold)
                    };
                    index = if goes_left { left } else { right };
                }
            }
        }
    }
}

/// The best boundary found in one leaf: codes up to `code` go left, and the missing bin goes where `missing_left` says.
#[derive(Debug, Clone, Copy, PartialEq)]
struct Split {
    input: usize,
    code: u8,
    missing_left: bool,
    gain: f64,
    left: Statistics,
    right: Statistics,
}

struct Leaf {
    node: usize,
    rows: Vec<usize>,
    statistics: Statistics,
    histogram: Histogram,
    split: Option<Split>,
}

/// What every tree in a fit shares: the quantized rows, each row's statistics, and the limits on growth.
pub struct Grower<'a> {
    pub bins: &'a Bins,
    pub codes: &'a Codes,
    pub statistics: &'a [Statistics],
    pub leaves: NonZeroUsize,
    pub minimum_rows_per_leaf: NonZeroUsize,
    pub penalty: Penalty,
}

impl Grower<'_> {
    /// Grows a tree over `rows` splitting only on `inputs`, then asks `leaf_value` for each leaf given its rows and
    /// their statistics; histograms are folded through `fold`, so any fold grows the same tree.
    pub fn grow(
        &self,
        rows: Vec<usize>,
        inputs: &[usize],
        fold: &impl Fold,
        leaf_value: impl Fn(&[usize], Statistics) -> Score,
    ) -> Tree {
        let histogram_of = |rows: &[usize]| {
            fold.fold(rows.len(), |range| {
                Histogram::of(self.codes, self.statistics, &rows[range], inputs)
            })
        };
        let statistics = concatenate(rows.iter().map(|&row| self.statistics[row]));
        let histogram = histogram_of(&rows);
        let split = self.best_split(&histogram, statistics, inputs);
        let mut nodes = vec![Node::Leaf {
            value: Score::default(),
        }];
        let mut leaves = vec![Leaf {
            node: 0,
            rows,
            statistics,
            histogram,
            split,
        }];
        while leaves.len() < self.leaves.get() {
            let chosen = leaves
                .iter()
                .enumerate()
                .filter_map(|(index, leaf)| leaf.split.map(|split| (index, split.gain)))
                .max_by(|first, second| first.1.total_cmp(&second.1));
            let Some((index, _)) = chosen else {
                break;
            };
            let parent = leaves.swap_remove(index);
            let split = parent.split.expect("a chosen leaf has a split");
            let (left_rows, right_rows): (Vec<usize>, Vec<usize>) =
                parent
                    .rows
                    .iter()
                    .partition(|&&row| match self.codes.row(row)[split.input] {
                        MISSING => split.missing_left,
                        code => code <= split.code,
                    });
            let left_smaller = left_rows.len() <= right_rows.len();
            let smaller = histogram_of(if left_smaller {
                &left_rows
            } else {
                &right_rows
            });
            let larger = parent.histogram.without(smaller.clone());
            let (left_histogram, right_histogram) = if left_smaller {
                (smaller, larger)
            } else {
                (larger, smaller)
            };
            let (left, right) = (nodes.len(), nodes.len() + 1);
            nodes[parent.node] = Node::Split {
                input: split.input,
                threshold: self.bins.upper(split.input, split.code),
                missing_left: split.missing_left,
                gain: split.gain,
                left,
                right,
            };
            nodes.extend([
                Node::Leaf {
                    value: Score::default(),
                },
                Node::Leaf {
                    value: Score::default(),
                },
            ]);
            for (node, rows, statistics, histogram) in [
                (left, left_rows, split.left, left_histogram),
                (right, right_rows, split.right, right_histogram),
            ] {
                let split = self.best_split(&histogram, statistics, inputs);
                leaves.push(Leaf {
                    node,
                    rows,
                    statistics,
                    histogram,
                    split,
                });
            }
        }
        for leaf in leaves {
            nodes[leaf.node] = Node::Leaf {
                value: leaf_value(&leaf.rows, leaf.statistics),
            };
        }
        Tree { nodes }
    }

    /// G² / (H + λ), the loss reduction a Newton step on these rows would buy.
    fn score(&self, statistics: Statistics) -> f64 {
        statistics.gradient().powi(2) / (statistics.hessian() + self.penalty.get())
    }

    fn best_split(
        &self,
        histogram: &Histogram,
        parent: Statistics,
        inputs: &[usize],
    ) -> Option<Split> {
        let minimum = self.minimum_rows_per_leaf.get() as u64;
        let parent_score = self.score(parent);
        let mut best: Option<Split> = None;
        for &input in inputs {
            let slots = histogram.input(input);
            let missing = slots[usize::from(MISSING)];
            let mut values_left = Statistics::empty();
            for (code, &slot) in slots.iter().enumerate().take(self.bins.count(input)) {
                values_left = values_left.combine(slot);
                for missing_left in [false, true] {
                    let left = if missing_left {
                        values_left.combine(missing)
                    } else {
                        values_left
                    };
                    let right = parent.without(left);
                    if left.count() < minimum || right.count() < minimum {
                        continue;
                    }
                    let gain = self.score(left) + self.score(right) - parent_score;
                    if gain > best.map_or(0.0, |best| best.gain) {
                        best = Some(Split {
                            input,
                            code: code as u8,
                            missing_left,
                            gain,
                            left,
                            right,
                        });
                    }
                }
            }
        }
        best
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::learning::loss::Derivatives;
    use crate::common::learning::matrix::Matrix;
    use crate::common::monoid::Sequential;

    /// Grows one tree on a single input whose leaves take the Newton step, -G / (H + λ).
    fn grow(
        values: &[f64],
        gradients: &[f64],
        leaves: usize,
        minimum_rows_per_leaf: usize,
    ) -> Tree {
        let matrix = Matrix::new(values, NonZeroUsize::MIN).unwrap();
        let bins = Bins::fit(&matrix);
        let codes = bins.quantize(&matrix);
        let statistics: Vec<Statistics> = gradients
            .iter()
            .map(|&gradient| {
                Statistics::of(Derivatives {
                    gradient,
                    hessian: 1.0,
                })
            })
            .collect();
        let penalty = Penalty::new(1e-9).unwrap();
        let grower = Grower {
            bins: &bins,
            codes: &codes,
            statistics: &statistics,
            leaves: NonZeroUsize::new(leaves).unwrap(),
            minimum_rows_per_leaf: NonZeroUsize::new(minimum_rows_per_leaf).unwrap(),
            penalty,
        };
        grower.grow(
            (0..values.len()).collect(),
            &[0],
            &Sequential,
            |_, statistics| {
                Score::new(-statistics.gradient() / (statistics.hessian() + penalty.get()))
            },
        )
    }

    #[test]
    fn test_a_stump_splits_where_the_gradient_changes_sign() {
        let tree = grow(&[1.0, 2.0, 3.0, 4.0], &[5.0, 5.0, -5.0, -5.0], 2, 1);
        assert_eq!(tree.nodes.len(), 3);
        match tree.nodes[0] {
            Node::Split {
                input,
                threshold,
                missing_left,
                gain,
                ..
            } => {
                assert_eq!((input, threshold, missing_left), (0, Some(2.5), false));
                assert!((gain - 100.0).abs() < 1e-6, "{gain}");
            }
            Node::Leaf { .. } => panic!("the root did not split"),
        }
        assert!((tree.score(&[1.0]).get() + 5.0).abs() < 1e-6);
        assert!((tree.score(&[9.0]).get() - 5.0).abs() < 1e-6);
    }

    #[test]
    fn test_missing_values_are_routed_to_the_side_they_resemble() {
        let tree = grow(
            &[f64::NAN, f64::NAN, 1.0, 2.0],
            &[-5.0, -5.0, 5.0, 5.0],
            2,
            1,
        );
        assert!((tree.score(&[f64::NAN]).get() - 5.0).abs() < 1e-6);
        assert!((tree.score(&[1.5]).get() + 5.0).abs() < 1e-6);
        assert!((tree.score(&[f64::INFINITY]).get() + 5.0).abs() < 1e-6);
        let text = serde_json::to_string(&tree).unwrap();
        assert_eq!(serde_json::from_str::<Tree>(&text).unwrap(), tree);
    }

    #[test]
    fn test_a_leaf_never_holds_fewer_rows_than_the_minimum_and_growth_stops_at_the_leaf_limit() {
        let values: Vec<f64> = (0..8).map(f64::from).collect();
        let gradients = [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0];
        assert_eq!(grow(&values, &gradients, 8, 5).nodes.len(), 1);
        assert_eq!(
            grow(&values, &[5.0, 5.0, 5.0, 5.0, -5.0, -5.0, -5.0, -5.0], 2, 4)
                .nodes
                .len(),
            3
        );
        assert_eq!(grow(&values, &gradients, 3, 1).nodes.len(), 5);
        assert_eq!(grow(&values, &[0.0; 8], 8, 1).nodes.len(), 1);
    }

    #[test]
    fn test_a_tree_refuses_a_child_that_is_not_after_its_parent() {
        let split = |left, right| Node::Split {
            input: 0,
            threshold: Some(0.0),
            missing_left: true,
            gain: 1.0,
            left,
            right,
        };
        let leaf = Node::Leaf {
            value: Score::new(1.0),
        };
        assert_eq!(Tree::try_from(vec![]), Err(TreeError::Empty));
        assert_eq!(
            Tree::try_from(vec![split(0, 1), leaf.clone()]),
            Err(TreeError::Child {
                node: 0,
                child: 0,
                length: 2
            })
        );
        assert_eq!(
            Tree::try_from(vec![split(1, 3), leaf.clone(), leaf.clone()]),
            Err(TreeError::Child {
                node: 0,
                child: 3,
                length: 3
            })
        );
        let tree = Tree::try_from(vec![
            split(1, 2),
            Node::Leaf {
                value: Score::new(-1.0),
            },
            leaf,
        ])
        .unwrap();
        let text = serde_json::to_string(&tree).unwrap();
        assert_eq!(serde_json::from_str::<Tree>(&text).unwrap(), tree);
        assert_eq!(
            (tree.score(&[f64::NAN]).get(), tree.score(&[0.5]).get()),
            (-1.0, 1.0)
        );
    }
}
