# Pocket Size Fund Task Manager

## setup
>
> Initial system setup and prerequisite configuration

```bash
set -euo pipefail

echo "Setting up development environment"

echo "Checking prerequisites"
missing_deps=()

if ! command -v docker >/dev/null >&1; then
    missing_deps+=("Docker")
fi

if [[ ${#missing_deps[@]} -gt 0 ]]; then
    echo "Missing prerequisites: ${missing_deps[*]}"
    echo "Please install the following:"
    for dep in "${missing_deps[@]}"; do
        case $dep in
            "Docker")
                echo "  - Docker: https://docs.docker.com/get-docker/"
                ;;
        esac
    done
    exit 1
fi

echo "Checking DockerHub authentication"
if ! docker info >/dev/null >&1; then
    echo "Docker daemon not running"
    exit 1
fi

if ! docker system info --format '{{.Username}}' 2>/dev/null | grep -q .; then
    echo "️Not logged into DockerHub. Run 'docker login' first"
    exit 1
fi

echo "Prerequisites check completed"
```

## dev
>
> Full development cycle (install, format, lint, test)

```sh

echo "Running full development workflow"
echo "==================================="

echo " Installing dependencies..."
mask development python install

echo ""
echo " Running quality checks..."
mask development quality

echo ""
echo " Running tests..."
mask development python test

echo ""
echo "Development workflow completed successfully!"
```

## infrastructure
>
> Manage infrastructure deployments
>
### base
>
> Base infrastructure deployment
>
#### up
>
> Deploy complete infrastructure stack (Lightsail + Docker Swarm) to local and production environments. Creates cloud resources, SSH configuration, Docker contexts, and deploys both infrastructure and application services.

```bash
cd infrastructure
uv run python -m infrastructure up
```

#### down
>
> Completely tear down infrastructure and services.

```bash
cd infrastructure

uv run python -m infrastructure down
echo "📝 Summary: All cloud resources destroyed, contexts removed, SSH config cleaned"
```

### monitoring

 deploy monitoring stack

#### deploy

```nu
cd infrastructure
$env.GRAFANA_ADMIN_PASSWORD | docker secret create GRAFANA_ADMIN_PASSWORD -
docker stack deploy -c stack.yml monitoring
```

### applications
>
> Build and deploy application containers
>
#### build
>
> Build and push application Docker images to DockerHub

```bash
set -euo pipefail

echo "️Building and pushing application images"

for app_dir in applications/*/; do
    app_name=$(basename "$app_dir")
    if [[ -f "$app_dir/Dockerfile" ]]; then
        echo "Building $app_name..."
        cd "$app_dir"

        version=$(uv version --short 2>/dev/null || echo "latest")

        docker build -t "pocketsizefund/$app_name:latest" -t "pocketsizefund/$app_name:$version" .

        docker push "pocketsizefund/$app_name:latest"
        docker push "pocketsizefund/$app_name:$version"

        cd ..
        echo "$app_name built and pushed"
    else
        echo "️Skipping $app_name (no Dockerfile found)"
    fi
done

echo "All application images built and pushed successfully"
```

#### deploy
>
> Deploy applications to both local and production Docker swarm

```nu
echo "Deploying applications"

cd applications
# $env.ALPACA_API_KEY | docker secret create ALPACA_API_KEY -
docker stack deploy -c stack.yml applications

echo "Application deployment completed"
```

## test
>
> Test application endpoints and service health across environments

### health
>
> Check Docker service health across all contexts

```bash
set -euo pipefail

test_service_health() {
    local context="$1"
    echo "Docker Services in $context:"

    if docker context ls --format '{{.Name}}' | grep -q "^${context}$"; then
        docker context use "$context" >/dev/null >&1

        if docker service ls --format "table {{.Name}}\t{{.Replicas}}\t{{.Image}}" >/dev/null; then
            echo "Services listed successfully"
        else
            echo "No services found or connection error"
        fi
    else
        echo "Context $context not available"
    fi
}

echo "Service health check"
test_service_health "pocketsizefund-local"
test_service_health "pocketsizefund-production"

docker context use default >/dev/null >&1 || true
```

### all
>
> Run complete test suite (endpoints + health checks)

```bash
set -euo pipefail

echo "Running complete test suite"

mask test endpoints
mask test health

echo "Complete test suite finished"
```

## docker
>
> Docker context and service management commands

### context
>
> Switch Docker context between local and production environments
>
#### local
>
> Switch to local Docker swarm context

```bash
docker context use pocketsizefund-local
echo "Switched to local context"
docker context ls | grep "\*"
```

#### production
>
> Switch to production Docker swarm context

```bash
docker context use pocketsizefund-production
echo "Switched to production context"
docker context ls | grep "\*"
```

#### default
>
> Switch back to default Docker context

```bash
docker context use default
echo "Switched to default context"
```

### services
>
> Docker swarm service management
>
#### ls
>
> List all Docker services with health status

```bash
echo "Docker services status:"
docker service ls
echo "Service health details:"
docker service ls --format "table {{.Name}}\t{{.Replicas}}\t{{.Image}}\t{{.Ports}}" | \
  while IFS=$'\t' read -r name replicas image ports; do
    if [[ "$name" != "NAME" ]]; then
      printf "%-30s %s\n" "$name" "$replicas"
    fi
  done
```

#### logs
>
> View logs for a specific service (interactive selection)

```bash
echo "Available services:"
services=($(docker service ls --format '{{.Name}}'))

if [[ ${#services[@]} -eq 0 ]]; then
  echo "No services found"
  exit 1
fi

echo "Select a service:"
select service in "${services[@]}"; do
  if [[ -n "$service" ]]; then
    echo "Showing logs for $service (press Ctrl+C to exit):"
    docker service logs -f "$service"
    break
  else
    echo "Invalid selection. Please try again."
  fi
done
```

#### inspect
>
> Inspect service configuration and status

```bash
echo "Available services:"
services=($(docker service ls --format '{{.Name}}'))

if [[ ${#services[@]} -eq 0 ]]; then
  echo "No services found"
  exit 1
fi

echo "Select a service to inspect:"
select service in "${services[@]}"; do
  if [[ -n "$service" ]]; then
    echo "Inspecting $service:"
    echo "Service tasks"
    docker service ps "$service"
    echo "Service details"
    docker service inspect "$service" --pretty
    break
  else
    echo "Invalid selection. Please try again."
  fi
done
```

### stack
>
> Docker stack operations for infrastructure and applications
>
#### ls
>
> List all deployed stacks

```bash
echo "Deployed Docker stacks:"
docker stack ls
```

#### ps
>
> Show tasks for infrastructure and application stacks

```bash
echo "️Infrastructure stack:"
docker stack ps infrastructure >/dev/null || echo "Infrastructure stack not deployed"
echo "Applications stack:"
docker stack ps applications >/dev/null || echo "Applications stack not deployed"
```

#### rm
>
> Remove infrastructure and application stacks

```bash
echo "Removing Docker stacks"
docker stack rm applications >/dev/null && echo "Applications stack removed" || echo "Applications stack not found"
docker stack rm infrastructure >/dev/null && echo "Infrastructure stack removed" || echo "Infrastructure stack not found"
echo "Waiting for cleanup"
sleep 5
echo "Stack removal completed"
```

## status
>
> Show comprehensive system status across all environments

```bash
set -euo pipefail

echo "System status"

echo "Docker contexts:"
docker context ls

echo "Docker stacks:"
if docker context use pocketsizefund-local >/dev/null >&1; then
    echo "Local stacks:"
    docker stack ls >/dev/null || echo "  No stacks deployed locally"
fi
echo ""
if docker context use pocketsizefund-production >/dev/null >&1; then
    echo "Production stacks:"
    docker stack ls >/dev/null || echo "  No stacks deployed in production"
fi

docker context use default >/dev/null >&1 || true
echo "Status check completed"
```

## secrets
>
> Manage Docker Swarm secrets for application configuration

### list
>
> List all Docker secrets (requires active swarm context)

```bash
echo "Docker secrets:"
if docker info --format '{{.Swarm.LocalNodeState}}' 2>/dev/null | grep -q active; then
    docker secret ls
else
    echo "Not connected to Docker swarm - switch context first:"
    echo "mask docker context local"
    echo "mask docker context production"
fi
```

### create
>
> Interactively create required Docker secrets

```bash
set -euo pipefail

if ! docker info --format '{{.Swarm.LocalNodeState}}' 2>/dev/null | grep -q active; then
    echo "Not connected to Docker swarm - switch context first:"
    echo "mask docker context local"
    echo "mask docker context production"
    exit 1
fi

echo "Creating Docker secrets"
echo "Leave blank to skip a secret"

secrets=(
    "GRAFANA_ADMIN_PASSWORD:Grafana admin password"
    "ALPACA_API_KEY:Alpaca API key"
    "ALPACA_API_SECRET:Alpaca API secret"
    "ALPACA_BASE_URL:Alpaca base URL"
    "EDGAR_USER_AGENT:EDGAR user agent string"
    "DATA_BUCKET:Data storage bucket name"
    "POLYGON_API_KEY:Polygon API key"
    "DUCKDB_ACCESS_KEY:DuckDB access key"
    "DUCKDB_SECRET:DuckDB secret"
    "WEIGHTS_AND_BIASES_API_KEY:Weights & Biases API key"
)

for secret_info in "${secrets[@]}"; do
    secret_name="${secret_info%%:*}"
    secret_desc="${secret_info#*:}"

    echo -n "Enter $secret_desc ($secret_name): "
    read -r secret_value

    if [[ -n "$secret_value" ]]; then
        if echo "$secret_value" | docker secret create "$secret_name" - >/dev/null; then
            echo "Created $secret_name"
        else
            echo "️  $secret_name already exists or creation failed"
        fi
    else
        echo "️Skipped $secret_name"
    fi
done

echo "Secret creation completed"
```

## development
>
> Development tools and code quality checks for Python and Rust

### rust
>
> Rust development workflow commands
>
#### check
>
> Check Rust compilation

```bash
set -euo pipefail

echo "Checking Rust compilation"
cargo check
echo "Rust compiled successfully"
```

#### format
>
> Format Rust code using cargo fmt

```bash
set -euo pipefail

echo "Formatting Rust code"
cargo fmt --all
echo "Rust code formatted successfully"
```

#### lint
>
> Run Rust code quality checks with clippy

```bash
set -euo pipefail

echo "Running Rust lint checks"
cargo clippy
echo "Rust linting completed successfully"
```

#### test
>
> Run Rust tests

```bash
set -euo pipefail

echo "Running Rust tests"
cargo test --workspace --verbose
echo "Rust tests completed successfully"
```

#### all
>
> Full Rust development checks (check, format, lint, test)

```bash
set -euo pipefail

echo "Running full Rust development workflow"
echo "======================================"

echo "Checking compilation..."
mask development rust check

echo ""
echo "Formatting code..."
mask development rust format

echo ""
echo "Running linter..."
mask development rust lint

echo ""
echo "Running tests..."
mask development rust test

echo ""
echo "Rust development workflow completed successfully!"
```

### python
>
> Python development workflow commands
>
#### install
>
> Install Python dependencies

```bash
set -euo pipefail

echo "Installing Python dependencies"
export COMPOSE_BAKE=true
uv sync --all-packages --all-groups

echo "Python dependencies installed successfully"
```

#### format
>
> Format Python code

```bash
set -euo pipefail

echo "Formatting Python code"
ruff format

echo "Python code formatted successfully"
```

#### dead-code
>
> Check for dead Python code using vulture (runs after formatting)

```bash
set -euo pipefail

echo "Running vulture dead code analysis"
uvx vulture \
    --min-confidence 80 \
    --exclude '.flox,.venv,target' \
    .

echo "Dead code check completed"
```

#### lint
>
> Run comprehensive Python code quality checks (includes formatting and dead code check)

```bash
set -euo pipefail

echo "Running Python lint checks"

echo "Running ruff linting"
ruff check \
    --output-format=github \
    .

echo "Python linting completed successfully"
```

#### test
>
> Run Python tests using Docker Compose with coverage reporting

```bash
set -euo pipefail

echo "Running Python tests"

mkdir -p coverage_output

echo "Cleaning up previous test runs"
docker compose --file tests.yaml down --volumes --remove-orphans

echo "️Building test containers"
docker compose --file tests.yaml build tests

echo "Running tests with coverage"
docker compose --file tests.yaml run --rm --no-TTY tests

echo "Cleaning up test containers"
docker compose --file tests.yaml down --volumes --remove-orphans

echo "Python tests completed successfully"
```

#### all
>
> Full Python development checks (install, format, lint, dead-code, test)

```bash
set -euo pipefail

echo "Running full Python development workflow"
echo "========================================"

echo "Installing dependencies..."
mask development python install

echo ""
echo "Formatting code..."
mask development python format

echo ""
echo "Running linter..."
mask development python lint

echo ""
echo "Checking for dead code..."
mask development python dead-code

echo ""
echo "Running tests..."
mask development python test

echo ""
echo "Python development workflow completed successfully!"
```

### quality
>
> Run all code quality checks across the entire project (Python and Rust)

```bash
set -euo pipefail

echo "Running full project quality checks"
echo "==================================="

echo ""
echo "Python quality checks:"
echo "----------------------"
mask development python all

echo ""
echo "Rust quality checks:"
echo "--------------------"
if [[ -f "Cargo.toml" ]]; then
    mask development rust all
else
    echo "No Rust project found (Cargo.toml missing)"
fi

echo ""
echo "All quality checks completed successfully!"
```

## logs
>
> Quick access to service logs across environments

### infrastructure
>
> View logs for infrastructure services (Grafana, Prometheus, Traefik)

```bash
echo "Infrastructure service logs"
echo "Select environment:"
select env in "local" "production"; do
    case $env in
        local|production)
            context="pocketsizefund-$env"
            docker context use "$context"
            echo "Infrastructure services in $env:"
            services=($(docker service ls --filter "name=infrastructure_" --format '{{.Name}}'))
            if [[ ${#services[@]} -eq 0 ]]; then
                echo "No infrastructure services found"
                exit 1
            fi
            echo "Select service:"
            select service in "${services[@]}"; do
                if [[ -n "$service" ]]; then
                    echo "Logs for $service (press Ctrl+C to exit):"
                    docker service logs -f "$service"
                    break
                fi
            done
            break
            ;;
        *)
            echo "Invalid selection"
            ;;
    esac
done
docker context use default >/dev/null >&1 || true
```

### applications
>
> View logs for application services (DataManager, PortfolioManager)

```bash
echo "Application service logs"
echo "Select environment:"
select env in "local" "production"; do
    case $env in
        local|production)
            context="pocketsizefund-$env"
            docker context use "$context"
            echo "Application services in $env:"
            services=($(docker service ls --filter "name=applications_" --format '{{.Name}}'))
            if [[ ${#services[@]} -eq 0 ]]; then
                echo "No application services found"
                exit 1
            fi
            echo "Select service:"
            select service in "${services[@]}"; do
                if [[ -n "$service" ]]; then
                    echo " Logs for $service (press Ctrl+C to exit):"
                    docker service logs -f "$service"
                    break
                fi
            done
            break
            ;;
        *)
            echo "Invalid selection"
            ;;
    esac
done
docker context use default >/dev/null >&1 || true
```
