# Fix Pull Request Feedback

> Programmatic feedback resolution

## Instructions

Analyze and address comments left on GitHub pull request: $ARGUMENTS.

Follow these steps:

- Accept the pull request ID from $ARGUMENTS and fetch outstanding feedback from GitHub using `gh pr view $ARGUMENTS --json comments,reviews` and `gh api repos/:owner/:repo/pulls/$ARGUMENTS/comments` to get comprehensive review data.
- Check the current branch state using git commands to understand what changes have already been made locally, helping avoid duplicate work.
- Parse the fetched feedback to identify unresolved, actionable comments that require code changes (filter out resolved comments, general approvals, or comments already addressed in current branch).
- Use TodoWrite to create a comprehensive checklist of all feedback items that need addressing, including the comment author, file location, and specific change requested.
- For each todo item in the checklist, read the relevant files, implement the requested changes, and mark the todo as in_progress then completed as you work through it.
- After implementing changes for each feedback item, run the appropriate validation command based on the language of the modified files: `mask development python all` for Python changes or `mask development rust all` for Rust changes.
- If Claude is in "accept edits" mode, continue through all changes automatically; if in "approve edits" mode, stop at each proposed change for user review and approval before proceeding.
- After all feedback has been addressed and validation commands have passed, generate a concise bullet point summary of all changes made, organized by file or by feedback topic.
- If validation commands fail, fix the issues and re-run the validation before moving to the next feedback item or generating the final summary.
