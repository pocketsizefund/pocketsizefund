# PocketSizeFund Task Manager

## setup
> Initial system setup and prerequisite configuration
```bash
set -euo pipefail

echo "🚀 Setting up PocketSizeFund development environment"
echo "=================================================="

# Check prerequisites
echo "🔍 Checking prerequisites..."
missing_deps=()

if ! command -v docker >/dev/null 2>&1; then
    missing_deps+=("Docker")
fi

if ! command -v pulumi >/dev/null 2>&1; then
    missing_deps+=("Pulumi CLI")
fi

if [[ ${#missing_deps[@]} -gt 0 ]]; then
    echo "❌ Missing prerequisites: ${missing_deps[*]}"
    echo ""
    echo "Please install the following:"
    for dep in "${missing_deps[@]}"; do
        case $dep in
            "Docker")
                echo "  - Docker: https://docs.docker.com/get-docker/"
                ;;
            "Pulumi CLI")
                echo "  - Pulumi CLI: https://www.pulumi.com/docs/get-started/install/"
                ;;
        esac
    done
    exit 1
fi

# Check Docker login
echo "🐳 Checking DockerHub authentication..."
if ! docker info >/dev/null 2>&1; then
    echo "❌ Docker daemon not running"
    exit 1
fi

if ! docker system info --format '{{.Username}}' 2>/dev/null | grep -q .; then
    echo "⚠️  Not logged into DockerHub. Run 'docker login' first"
    exit 1
fi

# Set up environment variable
echo "🌍 Setting up environment..."
if [[ -z "${ACME_EMAIL:-}" ]]; then
    echo "⚠️  ACME_EMAIL environment variable not set"
    echo "   Add this to your shell profile: export ACME_EMAIL=\"your-email@example.com\""
    read -p "Enter your email for ACME certificates: " email
    if [[ -n "$email" ]]; then
        export ACME_EMAIL="$email"
        echo "✅ ACME_EMAIL set to: $ACME_EMAIL"
        echo "   Add 'export ACME_EMAIL=\"$ACME_EMAIL\"' to your shell profile"
    fi
else
    echo "✅ ACME_EMAIL: $ACME_EMAIL"
fi

echo "✅ Prerequisites check completed"
echo ""
echo "📋 Next steps:"
echo "  mask secrets create        # Set up Docker secrets"
echo "  mask infrastructure base up # Deploy infrastructure"
echo "  mask development python install # Install Python dependencies"
echo ""
echo "📖 Service URLs (after deployment):"
echo "  - DataManager: http://[manager-ip]:8080"
echo "  - PortfolioManager: http://[manager-ip]:8081"
echo "  - Grafana: https://grafana.example.com"
echo "  - Portainer: https://[manager-ip]:9443"
```

## dev
> Full development cycle (install, format, lint, test)
```bash
set -euo pipefail

echo "🔄 Running full development workflow"
echo "==================================="

echo "1️⃣ Installing dependencies..."
mask development python install

echo ""
echo "2️⃣ Running quality checks..."
mask development quality

echo ""
echo "3️⃣ Running tests..."
mask development python test

echo ""
echo "✅ Development workflow completed successfully!"
```

## ci
> Continuous integration workflow
```bash
set -euo pipefail

echo "🤖 Running CI workflow"
echo "====================="

echo "1️⃣ Quality checks..."
mask development quality

echo ""
echo "2️⃣ Testing..."
mask development python test

echo ""
echo "3️⃣ Building applications..."
mask infrastructure applications build

echo ""
echo "✅ CI workflow completed successfully!"
```

## infrastructure
> Manage infrastructure deployments
### base
> Base infrastructure deployment
#### up
> Deploy complete infrastructure stack (Pulumi + Docker) to local and production environments. Creates cloud resources, SSH configuration, Docker contexts, and deploys both infrastructure and application services.
```bash
set -euo pipefail

echo "🚀 Starting infrastructure deployment..."

cd infrastructure

# Deploy infrastructure with Pulumi
echo "📡 Deploying infrastructure with Pulumi..."
if ! pulumi up --yes; then
    echo "❌ Pulumi deployment failed"
    exit 1
fi

echo "📋 Getting infrastructure outputs..."
MANAGER_IP=$(pulumi stack output managerIp | tr -d '\r\n')
if [[ -z "$MANAGER_IP" ]]; then
    echo "❌ Failed to get manager IP from Pulumi"
    exit 1
fi

echo "🔑 Setting up SSH configuration..."
pulumi stack output --show-secrets sshPrivateKeyPem | tr -d '\r' > swarm.pem
chmod 600 swarm.pem

# Verify SSH key format
if ! ssh-keygen -l -f swarm.pem >/dev/null 2>&1; then
    echo "❌ Invalid SSH key format"
    exit 1
fi

# Setup SSH config
SSH_CONFIG="$HOME/.ssh/config"
mkdir -p "$(dirname "$SSH_CONFIG")"

# Remove existing pocketsizefund-production host config
if [[ -f "$SSH_CONFIG" ]]; then
    sed -i.bak '/^Host pocketsizefund-production$/,/^Host /{ /^Host pocketsizefund-production$/d; /^Host /!d; }' "$SSH_CONFIG" || true
    sed -i.bak '${ /^Host /!d; }' "$SSH_CONFIG" || true
fi

# Add new SSH config
cat >> "$SSH_CONFIG" << EOF

Host pocketsizefund-production
  HostName $MANAGER_IP
  User ubuntu
  IdentityFile $PWD/swarm.pem
  IdentitiesOnly yes
  StrictHostKeyChecking accept-new
  ServerAliveInterval 60
  ServerAliveCountMax 3
EOF

echo "🔐 Updating SSH known hosts..."
# Remove old host key and add new one
ssh-keygen -R "$MANAGER_IP" >/dev/null 2>&1 || true
ssh-keyscan -H "$MANAGER_IP" >> "$HOME/.ssh/known_hosts" 2>/dev/null

# Test SSH connection and Docker
echo "🧪 Testing SSH connection..."
MAX_RETRIES=5
RETRY_COUNT=0

while [[ $RETRY_COUNT -lt $MAX_RETRIES ]]; do
    if ssh -o ConnectTimeout=10 pocketsizefund-production 'docker info -f "{{.ServerVersion}} {{.Swarm.LocalNodeState}}"' 2>/dev/null; then
        echo "✅ SSH connection successful"
        break
    else
        ((RETRY_COUNT++))
        echo "⏳ SSH connection attempt $RETRY_COUNT/$MAX_RETRIES failed, retrying in 5 seconds..."
        sleep 5
    fi
done

if [[ $RETRY_COUNT -eq $MAX_RETRIES ]]; then
    echo "❌ Failed to establish SSH connection after $MAX_RETRIES attempts"
    exit 1
fi

echo "🐳 Setting up Docker contexts..."

# Remove existing contexts safely
for context in pocketsizefund-production pocketsizefund-local; do
    if docker context ls --format '{{.Name}}' | grep -q "^${context}$"; then
        docker context use default >/dev/null 2>&1 || true
        docker context rm -f "$context" >/dev/null 2>&1 || true
    fi
done

# Create Docker contexts
if ! docker context create pocketsizefund-production --docker "host=ssh://pocketsizefund-production"; then
    echo "❌ Failed to create production Docker context"
    exit 1
fi

if ! docker context create pocketsizefund-local --docker "host=unix:///var/run/docker.sock"; then
    echo "❌ Failed to create local Docker context"
    exit 1
fi

# Initialize local swarm if needed
echo "🔄 Ensuring local Docker swarm is initialized..."
docker context use pocketsizefund-local
if ! docker info --format '{{.Swarm.LocalNodeState}}' | grep -q active; then
    docker swarm init --advertise-addr 127.0.0.1 >/dev/null 2>&1 || true
fi

# Deploy infrastructure stack to production
echo "🚀 Deploying infrastructure stack to production..."
docker context use pocketsizefund-production
if ! docker stack deploy -c stack.yml infrastructure --with-registry-auth; then
    echo "❌ Failed to deploy infrastructure stack to production"
    exit 1
fi

# Deploy infrastructure stack to local
echo "🏠 Deploying infrastructure stack to local..."
docker context use pocketsizefund-local
if ! docker stack deploy -c stack.yml infrastructure --with-registry-auth; then
    echo "❌ Failed to deploy infrastructure stack to local"
    exit 1
fi

# Deploy application services
echo "📱 Deploying application services..."
cd ../applications
# Deploy to production
echo "🌐 Deploying applications to production..."
docker context use pocketsizefund-production
if ! docker stack deploy -c stack.yml applications --with-registry-auth; then
    echo "❌ Failed to deploy applications to production"
    exit 1
fi

# Deploy to local
echo "🏠 Deploying applications to local..."
docker context use pocketsizefund-local
if ! docker stack deploy -c stack.yml applications --with-registry-auth; then
    echo "❌ Failed to deploy applications to local"
    exit 1
fi

# Show cluster status
echo "📊 Cluster status:"
docker context use pocketsizefund-production
echo "Production cluster:"
docker node ls 2>/dev/null || echo "  Unable to list production nodes"

docker context use pocketsizefund-local
echo "Local cluster:"
docker node ls 2>/dev/null || echo "  Unable to list local nodes"

echo "✅ Infrastructure deployment completed successfully!"
echo ""
echo "Next steps:"
echo "  mask test all                # Test complete deployment"
echo "  mask docker stack ps         # Check stack status"
echo "  mask docker context production  # Switch to production context"
```

#### down

> Completely tear down infrastructure and services. Removes Docker stacks, destroys Pulumi cloud resources, cleans up SSH configuration and Docker contexts.

```bash
set -euo pipefail

echo "🛑 Taking down infrastructure..."

cd infrastructure

echo "📱 Removing application stacks..."
for context in pocketsizefund-production pocketsizefund-local; do
    echo "  Removing from $context..."
    if docker context ls --format '{{.Name}}' | grep -q "^${context}$"; then
        docker context use "$context"
        docker stack rm applications 2>/dev/null || echo "    applications not found in $context"
        docker stack rm infrastructure 2>/dev/null || echo "    infrastructure stack not found in $context"
    else
        echo "    Context $context not found"
    fi
done

# Wait for services to be removed
echo "⏳ Waiting for services to stop..."
sleep 10

# Destroy Pulumi infrastructure
echo "☁️  Destroying cloud infrastructure..."
if ! pulumi destroy --yes; then
    echo "❌ Pulumi destroy failed"
    exit 1
fi

# Clean up SSH config
echo "🧹 Cleaning up SSH configuration..."
SSH_CONFIG="$HOME/.ssh/config"
if [[ -f "$SSH_CONFIG" ]]; then
    sed -i.bak '/^Host pocketsizefund-production$/,/^Host /{ /^Host pocketsizefund-production$/d; /^Host /!d; }' "$SSH_CONFIG" || true
    sed -i.bak '${ /^Host /!d; }' "$SSH_CONFIG" || true
fi

# Remove SSH key
rm -f swarm.pem

# Remove Docker contexts
echo "🐳 Removing Docker contexts..."
docker context use default >/dev/null 2>&1 || true
for context in pocketsizefund-production pocketsizefund-local; do
    docker context rm -f "$context" >/dev/null 2>&1 || true
done

echo "✅ Infrastructure taken down successfully!"
echo ""
echo "📝 Summary: All cloud resources destroyed, contexts removed, SSH config cleaned"
```

### applications
> Build and deploy application containers
#### build
> Build and push application Docker images to DockerHub
```bash
set -euo pipefail

echo "🏗️ Building and pushing application images..."

for app_dir in applications/*/; do
    app_name=$(basename "$app_dir")
    if [[ -f "$app_dir/Dockerfile" ]]; then
        echo "📦 Building $app_name..."
        cd "$app_dir"

        # Get version from uv
        version=$(uv version --short 2>/dev/null || echo "latest")

        # Build with both latest and version tags
        docker build -t "pocketsizefund/$app_name:latest" -t "pocketsizefund/$app_name:$version" .

        # Push both tags
        docker push "pocketsizefund/$app_name:latest"
        docker push "pocketsizefund/$app_name:$version"

        cd ..
        echo "✅ $app_name built and pushed"
    else
        echo "⚠️  Skipping $app_name (no Dockerfile found)"
    fi
done

echo "🎉 All application images built and pushed successfully!"
```

#### deploy
> Deploy applications to both local and production Docker swarm
```bash
set -euo pipefail

echo "🚀 Deploying applications..."

# Deploy to production context
if docker context ls --format '{{.Name}}' | grep -q "^pocketsizefund-production$"; then
    echo "🌐 Deploying to production..."
    docker context use pocketsizefund-production
    docker stack deploy -c applications/stack.yml applications --with-registry-auth
else
    echo "⚠️  Production context not found, skipping production deployment"
fi

# Deploy to local context
if docker context ls --format '{{.Name}}' | grep -q "^pocketsizefund-local$"; then
    echo "🏠 Deploying to local..."
    docker context use pocketsizefund-local
    docker stack deploy -c applications/stack.yml applications --with-registry-auth
else
    echo "⚠️  Local context not found, skipping local deployment"
fi

# Reset to default context
docker context use default >/dev/null 2>&1 || true

echo "✅ Application deployment completed!"
echo ""
echo "Next steps:"
echo "  mask test endpoints  # Test service endpoints"
echo "  mask docker services ls  # Check service status"
```

## test
> Test application endpoints and service health across environments

### endpoints
> Test HTTP endpoints for DataManager and PortfolioManager services
```bash
set -euo pipefail

echo "🧪 Testing Application Endpoints"
echo "================================="

# Function to test HTTP endpoints
test_endpoint() {
    local name="$1"
    local url="$2"
    local context="$3"

    printf "%-25s %-15s " "$name" "[$context]"

    if timeout 10 curl -sf "$url" >/dev/null 2>&1; then
        echo "✅ OK"
        return 0
    elif timeout 10 curl -s "$url" >/dev/null 2>&1; then
        echo "🔶 RESPONDING (non-200)"
        return 1
    else
        echo "❌ FAILED"
        return 1
    fi
}

# Get production manager IP
cd infrastructure
MANAGER_IP=""
if pulumi stack --show-name 2>/dev/null; then
    MANAGER_IP=$(pulumi stack output managerIp 2>/dev/null || echo "")
fi
cd ..

echo "🏠 Local endpoints:"
test_endpoint "DataManager" "http://localhost:8080/health" "local"
test_endpoint "DataManager (root)" "http://localhost:8080/" "local"
test_endpoint "PortfolioManager" "http://localhost:8081/health" "local"
test_endpoint "PortfolioManager (root)" "http://localhost:8081/" "local"

# Production tests (if manager IP available)
if [[ -n "$MANAGER_IP" ]]; then
    echo ""
    echo "🌐 Production endpoints (IP: $MANAGER_IP):"
    test_endpoint "DataManager" "http://$MANAGER_IP:8080/health" "production"
    test_endpoint "DataManager (root)" "http://$MANAGER_IP:8080/" "production"
    test_endpoint "PortfolioManager" "http://$MANAGER_IP:8081/health" "production"
    test_endpoint "PortfolioManager (root)" "http://$MANAGER_IP:8081/" "production"
else
    echo ""
    echo "⚠️  Production manager IP not available - skipping production tests"
fi

docker context use default >/dev/null 2>&1 || true
echo "✅ Endpoint testing completed"
```

### health
> Check Docker service health across all contexts
```bash
set -euo pipefail

test_service_health() {
    local context="$1"
    echo "🔍 Docker Services in $context:"

    if docker context ls --format '{{.Name}}' | grep -q "^${context}$"; then
        docker context use "$context" >/dev/null 2>&1

        if docker service ls --format "table {{.Name}}\t{{.Replicas}}\t{{.Image}}" 2>/dev/null; then
            echo "✅ Services listed successfully"
        else
            echo "  No services found or connection error"
        fi
    else
        echo "  Context $context not available"
    fi
    echo ""
}

echo "📊 Service Health Check"
echo "======================"
test_service_health "pocketsizefund-local"
test_service_health "pocketsizefund-production"

docker context use default >/dev/null 2>&1 || true
```

### all
> Run complete test suite (endpoints + health checks)
```bash
set -euo pipefail

echo "🧠 Running complete test suite..."
echo "===================================="

mask test endpoints
echo ""
mask test health

echo "🎉 Complete test suite finished"
```

## docker
> Docker context and service management commands

### context
> Switch Docker context between local and production environments
#### local
> Switch to local Docker swarm context
```bash
docker context use pocketsizefund-local
echo "✅ Switched to local context"
docker context ls | grep "\*"
```

#### production
> Switch to production Docker swarm context
```bash
docker context use pocketsizefund-production
echo "✅ Switched to production context"
docker context ls | grep "\*"
```

#### default
> Switch back to default Docker context
```bash
docker context use default
echo "✅ Switched to default context"
```

### services
> Docker swarm service management
#### ls
> List all Docker services with health status
```bash
echo "📊 Docker Services Status:"
docker service ls
echo ""
echo "📋 Service Health Details:"
docker service ls --format "table {{.Name}}\t{{.Replicas}}\t{{.Image}}\t{{.Ports}}" | \
  while IFS=$'\t' read -r name replicas image ports; do
    if [[ "$name" != "NAME" ]]; then
      printf "%-30s %s\n" "$name" "$replicas"
    fi
  done
```

#### logs
> View logs for a specific service (interactive selection)
```bash
echo "📋 Available services:"
services=($(docker service ls --format '{{.Name}}'))

if [[ ${#services[@]} -eq 0 ]]; then
  echo "❌ No services found"
  exit 1
fi

echo "Select a service:"
select service in "${services[@]}"; do
  if [[ -n "$service" ]]; then
    echo "📜 Showing logs for $service (press Ctrl+C to exit):"
    docker service logs -f "$service"
    break
  else
    echo "Invalid selection. Please try again."
  fi
done
```

#### inspect
> Inspect service configuration and status
```bash
echo "📋 Available services:"
services=($(docker service ls --format '{{.Name}}'))

if [[ ${#services[@]} -eq 0 ]]; then
  echo "❌ No services found"
  exit 1
fi

echo "Select a service to inspect:"
select service in "${services[@]}"; do
  if [[ -n "$service" ]]; then
    echo "🔍 Inspecting $service:"
    echo "--- Service Tasks ---"
    docker service ps "$service"
    echo ""
    echo "--- Service Details ---"
    docker service inspect "$service" --pretty
    break
  else
    echo "Invalid selection. Please try again."
  fi
done
```

### stack
> Docker stack operations for infrastructure and applications
#### ls
> List all deployed stacks
```bash
echo "📚 Deployed Docker Stacks:"
docker stack ls
```

#### ps
> Show tasks for infrastructure and application stacks
```bash
echo "🏗️ Infrastructure Stack:"
docker stack ps infrastructure 2>/dev/null || echo "Infrastructure stack not deployed"
echo ""
echo "📱 Applications Stack:"
docker stack ps applications 2>/dev/null || echo "Applications stack not deployed"
```

#### rm
> Remove infrastructure and application stacks
```bash
echo "🛑 Removing Docker stacks..."
docker stack rm applications 2>/dev/null && echo "✅ Applications stack removed" || echo "⚠️  Applications stack not found"
docker stack rm infrastructure 2>/dev/null && echo "✅ Infrastructure stack removed" || echo "⚠️  Infrastructure stack not found"
echo "🧹 Waiting for cleanup..."
sleep 5
echo "✅ Stack removal completed"
```

## status
> Show comprehensive system status across all environments
```bash
set -euo pipefail

echo "📊 PocketSizeFund System Status"
echo "==============================="
echo ""

# Docker contexts
echo "🐳 Docker Contexts:"
docker context ls
echo ""

# Pulumi stack info
echo "☁️  Infrastructure Status:"
cd infrastructure
if pulumi stack --show-name 2>/dev/null; then
    echo "Current stack: $(pulumi stack --show-name)"
    if MANAGER_IP=$(pulumi stack output managerIp 2>/dev/null); then
        echo "Manager IP: $MANAGER_IP"
    else
        echo "Manager IP: Not available (stack may be down)"
    fi
else
    echo "No active Pulumi stack found"
fi
cd ..
echo ""

# Docker stacks
echo "📚 Docker Stacks:"
if docker context use pocketsizefund-local >/dev/null 2>&1; then
    echo "Local stacks:"
    docker stack ls 2>/dev/null || echo "  No stacks deployed locally"
fi
echo ""
if docker context use pocketsizefund-production >/dev/null 2>&1; then
    echo "Production stacks:"
    docker stack ls 2>/dev/null || echo "  No stacks deployed in production"
fi

# Reset context
docker context use default >/dev/null 2>&1 || true
echo ""
echo "✅ Status check completed"
```

## secrets
> Manage Docker Swarm secrets for application configuration

### list
> List all Docker secrets (requires active swarm context)
```bash
echo "🔐 Docker Secrets:"
if docker info --format '{{.Swarm.LocalNodeState}}' 2>/dev/null | grep -q active; then
    docker secret ls
else
    echo "❌ Not connected to Docker swarm - switch context first:"
    echo "  mask docker context local"
    echo "  mask docker context production"
fi
```

### create
> Interactively create required Docker secrets
```bash
set -euo pipefail

if ! docker info --format '{{.Swarm.LocalNodeState}}' 2>/dev/null | grep -q active; then
    echo "❌ Not connected to Docker swarm - switch context first:"
    echo "  mask docker context local"
    echo "  mask docker context production"
    exit 1
fi

echo "🔐 Creating Docker Secrets"
echo "========================="
echo "Leave blank to skip a secret"
echo ""

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
        if echo "$secret_value" | docker secret create "$secret_name" - 2>/dev/null; then
            echo "✅ Created $secret_name"
        else
            echo "⚠️  $secret_name already exists or creation failed"
        fi
    else
        echo "⏭️  Skipped $secret_name"
    fi
    echo ""
done

echo "🔐 Secret creation completed"
```

## development
> Python development tools and code quality checks

### python
> Python development workflow commands
#### install
> Install Python dependencies using uv with all packages and groups
```bash
set -euo pipefail

echo "📦 Installing Python dependencies..."
export COMPOSE_BAKE=true
uv sync --all-packages --all-groups

echo "✅ Python dependencies installed successfully"
echo ""
echo "Next steps:"
echo "  mask development python format  # Format code"
echo "  mask development python lint    # Run quality checks"
echo "  mask development python test    # Run tests"
```

#### format
> Format Python code using ruff formatter
```bash
set -euo pipefail

echo "🎨 Formatting Python code..."
ruff format

echo "✅ Python code formatted successfully"
```

#### dead-code
> Check for dead Python code using vulture (runs after formatting)
```bash
set -euo pipefail

echo "🧹 Checking for dead Python code..."
mask development python format

echo "🔍 Running vulture dead code analysis..."
uvx vulture \
    --min-confidence 80 \
    --exclude '.flox,.venv,target' \
    .

echo "✅ Dead code check completed"
```

#### lint
> Run comprehensive Python code quality checks (includes formatting and dead code check)
```bash
set -euo pipefail

echo "🔍 Running Python code quality checks..."

# Run dead code check first (which includes formatting)
mask development python dead-code

echo "📋 Running ruff linting..."
ruff check \
    --output-format=github \
    .

# Note: ty check commented out in original
# uvx ty check

echo "✅ Python linting completed successfully"
```

#### test
> Run Python tests using Docker Compose with coverage reporting
```bash
set -euo pipefail

echo "🧪 Running Python tests..."

# Create coverage output directory
mkdir -p coverage_output

# Clean up any existing test containers
echo "🧹 Cleaning up previous test runs..."
docker compose --file tests.yaml down --volumes --remove-orphans

# Build test containers
echo "🏗️ Building test containers..."
docker compose --file tests.yaml build tests

# Run tests
echo "🚀 Running tests with coverage..."
docker compose --file tests.yaml run --rm --no-TTY tests

# Clean up after tests
echo "🧹 Cleaning up test containers..."
docker compose --file tests.yaml down --volumes --remove-orphans

echo "✅ Python tests completed successfully"
echo "📊 Coverage report available in coverage_output/.python_coverage.xml"
```

### quality
> Run all code quality checks across the entire project
```bash
set -euo pipefail

echo "🔍 Running comprehensive code quality checks..."

# Run Python quality checks
mask development python lint

# Run additional linting tools
echo "📋 Running additional linters..."
nu linter.nu
yamllint -d "{extends: relaxed, rules: {line-length: {max: 110}}}" .

echo "✅ All quality checks completed successfully"
echo ""
echo "🎉 Code is ready for review!"
```

## logs
> Quick access to service logs across environments

### infrastructure
> View logs for infrastructure services (Grafana, Prometheus, Traefik)
```bash
echo "📋 Infrastructure Service Logs"
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
                    echo "📜 Logs for $service (press Ctrl+C to exit):"
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
docker context use default >/dev/null 2>&1 || true
```

### applications
> View logs for application services (DataManager, PortfolioManager)
```bash
echo "📋 Application Service Logs"
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
                    echo "📜 Logs for $service (press Ctrl+C to exit):"
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
docker context use default >/dev/null 2>&1 || true
```
