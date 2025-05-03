# Consider sharing long GHC option sets via a common stanza

The same lengthy ghc-options string is duplicated three times. A custom common warnings stanza (cabal ≥ 2.2) avoids duplication and eases maintenance.
