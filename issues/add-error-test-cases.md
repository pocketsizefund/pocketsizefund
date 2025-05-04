# Add error case tests

There are no tests for error conditions. Consider adding tests that verify proper error handling.

platform/test/Spec.hs

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
