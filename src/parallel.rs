//! Folds a monoid across threads, one contiguous fragment of the range each.

use std::num::NonZeroUsize;
use std::ops::Range;

use crate::common::monoid::{Fold, Monoid, concatenate};

/// The fewest indices worth a thread of their own; a shorter range runs on fewer threads.
const MINIMUM_FRAGMENT: usize = 4_096;

/// Splits a range into at most this many fragments, folds each on its own thread, and combines them in order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Threads(NonZeroUsize);

impl Threads {
    pub fn new(count: NonZeroUsize) -> Self {
        Self(count)
    }

    /// One thread per core the process may use, or one when that cannot be read.
    pub fn available() -> Self {
        Self(std::thread::available_parallelism().unwrap_or(NonZeroUsize::MIN))
    }
}

impl Fold for Threads {
    fn fold<M: Monoid + Send>(
        &self,
        length: usize,
        fragment: impl Fn(Range<usize>) -> M + Sync,
    ) -> M {
        if length == 0 {
            return fragment(0..0);
        }
        let size = length.div_ceil(self.0.get()).max(MINIMUM_FRAGMENT);
        let fragment = &fragment;
        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..length)
                .step_by(size)
                .map(|start| scope.spawn(move || fragment(start..(start + size).min(length))))
                .collect();
            concatenate(
                handles
                    .into_iter()
                    .map(|handle| handle.join().expect("a fragment panicked")),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;
    use crate::common::monoid::Sequence;

    #[test]
    fn test_an_empty_range_is_one_empty_fragment() {
        let fragments = Threads::new(NonZeroUsize::new(4).unwrap())
            .fold(0, |range| Sequence(vec![(range.start, range.end)]));
        assert_eq!(fragments, Sequence(vec![(0, 0)]));
    }

    proptest! {
        #[test]
        fn property_threads_fold_every_index_once_in_order(length in 0_usize..20_000, threads in 1_usize..12) {
            let folded = Threads::new(NonZeroUsize::new(threads).unwrap()).fold(length, |range| Sequence(range.collect()));
            prop_assert_eq!(folded, Sequence((0..length).collect()));
        }
    }
}
