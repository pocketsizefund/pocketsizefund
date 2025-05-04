## add yaml linting to flox/mise

infrastructure/Pulumi.yaml (1)

1-3: Add a trailing newline at end of file.
YAML linters and many editors expect a single newline character after the last line. Please ensure there’s a newline after line 3 to satisfy the new-line-at-end-of-file lint rule.

## suggested fix
add `yamllint` to as a flox dependency and in the .mise.toml create a yaml lint task similar to haskell and python and put it in the main lint task.
