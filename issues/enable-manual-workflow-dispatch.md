# Enable manual workflow dispatch.

.github/workflows/stale.yaml

Consider extending the on: section to include:

```diff
workflow_dispatch:
```

This allows on-demand execution of this workflow via the GitHub Actions UI for debugging or one-off cleanups.
