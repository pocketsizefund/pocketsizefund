# Consider Python version compatibility

Python 3.12 is relatively new and might limit where this code can run. If not using 3.12-specific features, consider supporting earlier versions.

infrastructure/pyproject.toml

```diff
[project]
name = "infrastructure"
version = "0.1.0"
-requires-python = ">=3.12"
+requires-python = ">=3.10"
dependencies = [
    "pulumi>=3.162.0",
    "pulumi-gcp>=8.25.1",
]
```
