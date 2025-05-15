# PocketSizeFund Assistant Memory

Always use mise tasks for running tests, linting, formatting. If there is not a command already present in .mise.toml, suggest creating one.

## Code Style Guidelines

Unless the code is complex, never use comments in the code. The code should speak for itself.

## GitHub Issue Creation

When asked to create a new GitHub issue:

1. Follow the structure in template files:
  - `.github/ISSUE_TEMPLATE/BUG.md.md` for bugs
  - `.github/ISSUE_TEMPLATE/FEATURE.md` for new features
2. Include:
  - A descriptive title (without [FEATURE|BUG] prefix)
  - Explain why the feature is needed
  - Detail the benefits it will bring
  - Propose two implementation options (after considering at least five)
  - For each option, include:
    - Files that need updating
    - Code diff of proposed changes
    - Cost-benefit analysis

## GitHub Issue Workflow

1. After creating a ticket, it will be reviewed and possibly modified
2. Issues with "todo" status are prepared for implementation
3. When asked to look at ready tickets ready for implementation, search for Github issues with "in progress" status
