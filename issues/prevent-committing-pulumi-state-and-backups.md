# Prevent committing Pulumi state and backups

This file tracks only configuration, but Pulumi generates state directories (.pulumi/) and backup files (e.g., Pulumi.<stack>.yaml.backup). Update your .gitignore to exclude those:

```diff
.pulumi/
Pulumi.*.backup
```
