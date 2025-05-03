# Add manual trigger and concurrency control

To allow manual runs and prevent overlapping executions, extend the root of the workflow:

.github/workflows/test.yaml

```diff
on:
  pull_request:
    types: [opened, synchronize, reopened]
  workflow_dispatch:

concurrency:
  group: test-${{ github.ref }}
  cancel-in-progress: true
```

The job key format actually runs tests. Rename it to test (e.g., jobs:\n  test:) to better reflect its purpose and avoid confusion.
