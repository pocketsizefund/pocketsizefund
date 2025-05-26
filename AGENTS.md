# Codex Agent Instructions

This project uses **mise** tasks for code quality and testing. Before committing, run the following steps:

1. Install dependencies with `mise run python:install`.
2. Format code with `mise run python:format`.
3. Run lint checks with `mise run python:lint`.
4. Execute tests with `mise run python:test`.

## Code Style

- Write tested, self-documenting code. Avoid comments unless the code is complex.

## Pull Requests

- Use the Pull Request template: `.github/PULL_REQUEST_TEMPLATE.md`.

## GitHub Issues

- When creating an issue, use the templates under `.github/ISSUE_TEMPLATE`.
- Include a clear title, the reason the feature or fix is needed, and two implementation options with cost-benefit analysis.
