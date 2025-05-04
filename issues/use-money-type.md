# Consider stronger typing for monetary and numeric values.

Many fields like buyingPower, cash, equity, etc. are stored as String when they appear to represent monetary or numeric values. This might lead to runtime errors if these strings are parsed incorrectly later.

Consider using a more appropriate type like Double, Decimal, or a custom Money type that properly handles currency operations and precision.

platform/src/Account.hs

```diff
data Account = Account
  { id_ :: String,
    adminConfigurations :: AdminConfigurations,
    userConfigurations :: Maybe String,
    accountNumber :: String,
    status :: Status,
    cryptoStatus :: CryptoStatus,
    currency :: String,
-    buyingPower :: String,
+    buyingPower :: Double,
-    regulationBuyingPower :: String,
+    regulationBuyingPower :: Double,
    -- Similar changes for other monetary fields
```

## suggested solution
use the money package
https://hackage.haskell.org/package/safe-money-0.9.1/docs/Money.html
