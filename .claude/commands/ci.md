# CI Command

Run continuous integration workflow with automatic issue resolution.

## Usage

```bash
/ci
```

## Description

This command executes the full CI pipeline (`mask ci`) and automatically attempts to fix any issues that arise during:

1. **Quality Checks** - Format, lint, and dead code analysis
2. **Testing** - Run complete test suite with coverage
3. **Building** - Build and validate application containers

If any step fails, Claude will:
- Analyze the error output
- Identify the root cause
- Apply appropriate fixes (code formatting, dependency issues, test failures, etc.)
- Re-run the failed step to verify the fix
- Continue with the remaining CI steps

The command ensures all quality gates pass before completing, making it safe for deployment or merge requests.

## Implementation

The command will:
1. Run `mask ci` to execute the full CI workflow
2. Monitor output for failures or warnings
3. Automatically resolve common issues:
   - Code formatting violations
   - Linting errors
   - Import/dependency problems
   - Test failures
   - Build/container issues
4. Re-run failed steps after applying fixes
5. Report final CI status and any unresolved issues

This provides an automated "fix and retry" approach to CI, reducing manual intervention while maintaining code quality standards.