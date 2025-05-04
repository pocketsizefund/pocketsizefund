# Specify required permissions for labeling and closing.

The actions/stale@v9 action requires explicit write permissions for issues and pull requests to function correctly. Without this, attempts to mark or close stale items may silently fail. Add a permissions: block at the top of the workflow:

in .github/workflows/stale.yaml

```yaml
permissions:
  issues: write
  pull-requests: write
```
