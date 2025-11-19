# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Core Development
- **Install dependencies**: `mask development python install` (after `flox activate`)
- **Format code**: `mask development python format` (uses ruff)
- **Lint code**: `mask development python lint` (ruff with comprehensive ruleset)
- **Run tests**: `mask development python test` (pytest with coverage)
- **Check for dead code**: `mask development python dead-code` (vulture)
- **Run all quality checks**: `mask development quality`

### Testing
- **Run tests with coverage**: `uv run coverage run --parallel-mode -m pytest && uv run coverage combine && uv run coverage report`
- **Single test file**: `uv run pytest applications/*/tests/test_<name>.py`
- **Coverage output**: Available in `coverage_output/.python_coverage.xml`

### Infrastructure & Deployment
- **Deploy infrastructure**: `mask infrastructure base up`
- **Teardown infrastructure**: `mask infrastructure base down` 
- **Deploy applications**: `mask applications up`
- **Test endpoints**: `mask test`
- **Check Docker contexts**: `docker context ls`

### Development Environment
- **Environment manager**: Flox (`flox activate` to enter development shell)
- **Package manager**: uv (workspace with `applications/*` and `libraries/python`)
- **Python version**: 3.12.10 (strict requirement across all projects)

## Architecture Overview

### Workspace Structure
This is a **UV workspace** with multiple applications and shared libraries:

- **Root workspace** (`pyproject.toml`): Main project configuration with workspace members
- **Applications** (`applications/*/`): Microservices deployed as Docker containers
  - `datamanager`: Data collection service (FastAPI on port 8080)
  - `portfoliomanager`: Portfolio prediction service (FastAPI on port 8081)  
  - `models`: ML model training and data processing workflows
- **Libraries** (`libraries/python/`): Shared `internal` package with common utilities
- **Infrastructure** (`infrastructure/`): Pulumi-based cloud deployment

### Shared Internal Library (`libraries/python/src/internal/`)
Core components used across applications:
- **ML Components**: TFT models, LSTM/MHSA networks, loss functions
- **Data Types**: Equity bars, data class, types
- **Utilities**: Date handling

### Deployment Architecture
- **Local Development**: Docker Swarm on localhost
- **Production**: Pulumi-managed cloud infrastructure with Docker Swarm
- **Container Registry**: DockerHub (`pocketsizefund/*` images)
- **Monitoring**: Grafana, Prometheus, Portainer
- **Networking**: Traefik reverse proxy with Let's Encrypt TLS

### Service Communication
- **HTTP APIs**: FastAPI applications with health endpoints (`/health`)
- **Cloud Events**: Standardized event format for inter-service communication
- **Docker Networks**: Overlay networks (`public`, `internal`, `app-network`)

## Code Standards

### Python Configuration
- **Formatter**: Ruff (replaces black/isort)
- **Linter**: Ruff with comprehensive ruleset (90+ rule categories enabled)
- **Type Checking**: Pyright with relaxed import resolution
- **Testing**: Pytest with strict configuration
- **Coverage**: Line coverage tracking with parallel execution

### Key Dependencies
- **Web Framework**: FastAPI (consistent across services)
- **ML Stack**: TinyGrad, NumPy, Polars
- **Data**: PyArrow, Polars for data processing
- **Cloud**: Boto3, Azure libraries, Google Cloud SDK
- **Monitoring**: Structlog for structured logging

## Development Principles

From the project README, the team follows these principles:
- Test in production
- Always roll forward  
- Systems over process
- No code is good code
- Never write documentation
- Git is truth

## Infrastructure Secrets

Required Docker Swarm secrets for deployment:
- `GRAFANA_ADMIN_PASSWORD`
- `ALPACA_API_KEY_ID`, `ALPACA_API_SECRET`, `ALPACA_BASE_URL`
- `EDGAR_USER_AGENT`, `DATA_BUCKET`
- `POLYGON_API_KEY`, `DUCKDB_ACCESS_KEY`, `DUCKDB_SECRET`
- `WEIGHTS_AND_BIASES_API_KEY`

Create with: `echo "value" | docker secret create SECRET_NAME -`

## Common Workflow

1. **Setup**: `flox activate && mise run python:install`
2. **Develop**: Make changes to application or shared library code
3. **Quality**: `mise run lint` (format, lint, test, dead code check)
4. **Local Deploy**: `mask infrastructure base up` (deploys to both local and production)
5. **Test**: `mask test` (validates service endpoints)
6. **Monitor**: Access Grafana, Portainer, or service logs via Docker contexts
