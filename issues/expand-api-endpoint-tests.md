# Expand API endpoint tests

platform/test/Spec.hs

```haskell
  describe "GET /health" $ do
    it "responds with 200" $ do
      get "/health" `shouldRespondWith` 200
```
