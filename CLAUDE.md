# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Python: No specific build command (simple project)
- Haskell: `stack build` in platform/ directory
- Docker: `docker compose up -d` in platform/ directory

## Test Commands
- Haskell: `stack test` (runs all tests)
- Haskell: `stack test --test-arguments="--match PATTERN"` (runs specific tests)

## Lint/Format Commands
- Haskell: Style enforced via GHC options (-Wall, -Wcompat, etc.)

## Style Guidelines
- Python: Standard Python PEP 8 conventions
- Haskell:
  - Use 2-space indentation
  - Explicit exports in modules
  - Standard language extensions (DataKinds, TemplateHaskell, TypeOperators)
  - OverloadedStrings for Text handling
  - Explicit type signatures
  - Use Servant for web API definitions

## Error Handling
- Use proper error types and handle exceptions appropriately
- Environment variables validation at startup

## Project Structure
- Python service in root directory
- Haskell web service in platform/ directory
- Infrastructure configuration in infrastructure/ directory