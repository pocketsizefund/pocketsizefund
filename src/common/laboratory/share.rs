//! A share of a whole, such as the outcomes a band left below it, held between zero and one by construction.

use crate::common::learning::loss::Level;

/// A part over its whole, between zero and one inclusive.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Share(f64);

impl Share {
    /// `part` of `whole`, or `None` over an empty whole, which measures nothing.
    pub fn of(part: u64, whole: u64) -> Option<Self> {
        assert!(
            part <= whole,
            "a part of {part} exceeds its whole of {whole}"
        );
        (whole > 0).then(|| Self(part as f64 / whole as f64))
    }

    /// The rest of the whole.
    pub fn complement(self) -> Self {
        Self(1.0 - self.0)
    }

    pub fn get(self) -> f64 {
        self.0
    }
}

impl From<Level> for Share {
    fn from(level: Level) -> Self {
        Self(f64::from(level))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a_share_is_its_part_over_its_whole_and_none_over_nothing() {
        assert_eq!(Share::of(1, 4).map(Share::get), Some(0.25));
        assert_eq!(Share::of(4, 4).map(Share::get), Some(1.0));
        assert_eq!(Share::of(0, 0), None);
        assert_eq!(Share::of(1, 4).unwrap().complement().get(), 0.75);
        assert_eq!(Share::from(Level::try_from(0.1).unwrap()).get(), 0.1);
    }

    #[test]
    #[should_panic(expected = "a part of 5 exceeds its whole of 4")]
    fn test_a_part_larger_than_its_whole_is_refused() {
        Share::of(5, 4);
    }
}
