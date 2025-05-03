# code review

## Add error test cases
from coderabbit

platform/test/Spec.hs

```haskell
 describe "GET /health" $ do
    it "responds with 200" $ do
      get "/health" `shouldRespondWith` 200
 ```

There are no tests for error conditions. Consider adding tests that verify proper error handling.

```haskell
describe "GET /health" $ do
  it "responds with 200" $ do
    get "/health" `shouldRespondWith` 200

  -- If applicable, test error scenarios
  it "handles server errors appropriately" $ do
    -- You may need to mock dependencies to force errors
    -- This is pseudocode and would need to be adapted to your testing setup
    withMockedDependencyFailure $ do
      get "/health" `shouldRespondWith` 500
```

## Pin package versions to ensure reproducible builds
The four new dependencies currently have no version bounds (or, in the case of wreq, only a single deprecated release). To avoid surprises as new releases arrive—and to skip the deprecated wreq-0.5.4.1—please tighten the ranges in platform/package.yaml (around lines 19–22):

```yaml
- wreq
- lens
- bytestring
- scientific
+ wreq       >= 0.5.3 && < 0.6 && /= 0.5.4.1
+ lens       >= 5.0   && < 6
+ bytestring >= 0.11  && < 0.12
+ scientific >= 0.3.7 && < 0.4
```

• wreq has no preferred ranges but marks 0.5.4.1 as deprecated—exclude it explicitly.
• lens, bytestring and scientific have no preferred/deprecated versions, so these bounds will lock major releases and guard against breaking changes.

# Upgrade GHC image to latest stable version (9.12.2)

The Dockerfile is currently pulling haskell:9.8.4, but GHC 9.12.2 was released on March 14, 2025 with important bug-fixes. To keep your build environment current and in sync with development, update the base image:

• platform/Dockerfile:

```diff
-FROM haskell:9.8.4 AS builder
+FROM haskell:9.12.2 AS builder
```

## Returning 404 on JSON-decode error misrepresents the failure
platform/src/Lib.hs
```haskell
  key <- liftIO $ getEnv "ALPACA_API_KEY"
  secret <- liftIO $ getEnv "ALPACA_API_SECRET"
  baseUrl <- liftIO $ getEnv "ALPACA_BASE_URL"

  let opts =
        defaults
          & header "APCA-API-KEY-ID" .~ [pack key]
          & header "APCA-API-SECRET-KEY" .~ [pack secret]
      url = baseUrl ++ "/v2/account"
```

A decode failure is an internal/parsing issue, not “resource not found”. Use err502 (bad upstream) or err500 to reflect server-side trouble and avoid misleading clients/caches.

```diff
-      throwError $ err404 {errBody = "Alpaca account not found or unparseable"}
+      throwError $ err502 {errBody = "Failed to parse Alpaca /v2/account response"}
```
### suggested change
```haskell
  case eitherDecode body of
    Right account -> return account
    Left err -> do
      liftIO $ putStrLn "Error decoding account:"
      liftIO $ print err
      liftIO $ putStrLn $ "Raw response: " ++ BL8.unpack body
      throwError $ err404 {errBody = "Alpaca account not found or unparseable"}
  case eitherDecode body of
    Right account -> return account
    Left err -> do
      liftIO $ putStrLn "Error decoding account:"
      liftIO $ print err
      liftIO $ putStrLn $ "Raw response: " ++ BL8.unpack body
      throwError $ err502 {errBody = "Failed to parse Alpaca /v2/account response"}
```
