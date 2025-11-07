# Pocket Size Fund Task Manager

## setup

> Initial system setup and prerequisite configuration

```bash
set -euo pipefail

echo "Setting up development environment"

echo "Checking prerequisites"
missing_deps=()

if ! command -v docker >/dev/null >&1; then
    missing_deps+=("Docker")
fi

if ! command -v pulumi >/dev/null >&1; then
    missing_deps+=("Pulumi CLI")
fi

if [[ ${#missing_deps[@]} -gt 0 ]]; then
    echo "Missing prerequisites: ${missing_deps[*]}"
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

## continuous_integration

> Continuous integration workflow

```bash
set -euo pipefail

echo "Running continuous integration workflow"

echo "Testing"
mask development python test

echo "Building applications"
mask infrastructure applications build

echo "Continuous integration workflow completed successfully"
```

## infrastructure

> Manage infrastructure deployments

### base

> Base infrastructure deployment

#### up

> Deploy complete infrastructure stack (Pulumi + Docker) to local and production environments.

```bash
set -euo pipefail

echo "Starting infrastructure deployment"

cd infrastructure

echo "Deploying infrastructure with Pulumi"
if ! pulumi up --yes; then
    echo "Pulumi deployment failed"
    exit 1
fi

echo "Getting infrastructure outputs"
MANAGER_IP=$(pulumi stack output managerIp | tr -d '\r\n')
if [[ -z "$MANAGER_IP" ]]; then
    echo "Failed to get manager IP from Pulumi"
    exit 1
fi

echo "Setting up SSH configuration"
pulumi stack output --show-secrets sshPrivateKeyPem | tr -d '\r' > swarm.pem
chmod 600 swarm.pem
trap 'rm -f "$PWD/swarm.pem"' EXIT

if ! ssh-keygen -l -f swarm.pem >/dev/null >&1; then
    echo "Invalid SSH key format"
    exit 1
fi

SSH_CONFIG="$HOME/.ssh/config"
mkdir -p "$(dirname "$SSH_CONFIG")"

if [[ -f "$SSH_CONFIG" ]]; then
    sed -i.bak '/^Host pocketsizefund-production$/,/^Host /{ /^Host pocketsizefund-production$/d; /^Host /!d; }' "$SSH_CONFIG" || true
    sed -i.bak '${ /^Host /!d; }' "$SSH_CONFIG" || true
fi

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

echo "Updating SSH known hosts"
ssh-keygen -R "$MANAGER_IP" >/dev/null >&1 || true
ssh-keyscan -H "$MANAGER_IP" 2>/dev/null >> "$HOME/.ssh/known_hosts"

echo "Testing SSH connection"
MAX_RETRIES=5
RETRY_COUNT=0

while [[ $RETRY_COUNT -lt $MAX_RETRIES ]]; do
    if ssh -o ConnectTimeout=10 pocketsizefund-production 'docker info -f "{{.ServerVersion}} {{.Swarm.LocalNodeState}}"' 2>/dev/null; then
        echo "SSH connection successful"
        break
    else
        ((RETRY_COUNT++))
        echo "SSH connection attempt $RETRY_COUNT/$MAX_RETRIES failed, retrying in 5 seconds"
        sleep 5
    fi
done

if [[ $RETRY_COUNT -eq $MAX_RETRIES ]]; then
    echo "Failed to establish SSH connection after $MAX_RETRIES attempts"
    exit 1
fi

echo "Setting up Docker contexts"

for context in pocketsizefund-production pocketsizefund-local; do
    if docker context ls --format '{{.Name}}' | grep -q "^${context}$"; then
        docker context use default >/dev/null >&1 || true
        docker context rm -f "$context" >/dev/null >&1 || true
    fi
done

if ! docker context create pocketsizefund-production --docker "host=ssh://pocketsizefund-production"; then
    echo "Failed to create production Docker context"
    exit 1
fi

if ! docker context create pocketsizefund-local --docker "host=unix:///var/run/docker.sock"; then
    echo "Failed to create local Docker context"
    exit 1
fi

echo "Ensuring local Docker swarm is initialized"
docker context use pocketsizefund-local
if ! docker info --format '{{.Swarm.LocalNodeState}}' | grep -q active; then
    docker swarm init --advertise-addr 127.0.0.1 >/dev/null 2>&1 || true
fi

echo "Deploying infrastructure stack to production"
docker context use pocketsizefund-production
if ! docker stack deploy -c stack.yml infrastructure --with-registry-auth; then
    echo "Failed to deploy infrastructure stack to production"
    exit 1
fi

echo "Deploying infrastructure stack to local"
docker context use pocketsizefund-local
if ! docker stack deploy -c stack.yml infrastructure --with-registry-auth; then
    echo "Failed to deploy infrastructure stack to local"
    exit 1
fi

echo "Deploying application services"
cd ../applications
echo "Deploying applications to production"
docker context use pocketsizefund-production
if ! docker stack deploy -c stack.yml applications --with-registry-auth; then
    echo "Failed to deploy applications to production"
    exit 1
fi

echo "Deploying applications to local"
docker context use pocketsizefund-local
if ! docker stack deploy -c stack.yml applications --with-registry-auth; then
    echo "Failed to deploy applications to local"
    exit 1
fi

echo "Cluster status:"
docker context use pocketsizefund-production
echo "Production cluster:"
docker node ls >/dev/null || echo " Unable to list production nodes"

docker context use pocketsizefund-local
echo "Local cluster:"
docker node ls >/dev/null || echo " Unable to list local nodes"

echo "Infrastructure deployment completed successfully"
```

#### down

> Completely tear down infrastructure and services.

```bash
set -euo pipefail

echo "Taking down infrastructure"

cd infrastructure

echo "Removing application stacks"
for context in pocketsizefund-production pocketsizefund-local; do
    echo "Removing from $context"
    if docker context ls --format '{{.Name}}' | grep -q "^${context}$"; then
        docker context use "$context"
        docker stack rm applications >/dev/null || echo " applications not found in $context"
        docker stack rm infrastructure >/dev/null || echo " infrastructure stack not found in $context"
    else
        echo "Context $context not found"
    fi
done

echo "Waiting for services to stop"
sleep 10

echo "️Destroying cloud infrastructure"
if ! pulumi destroy --yes; then
    echo "Pulumi destroy failed"
    exit 1
fi

echo "Cleaning up SSH configuration"
SSH_CONFIG="$HOME/.ssh/config"
if [[ -f "$SSH_CONFIG" ]]; then
    sed -i.bak '/^Host pocketsizefund-production$/,/^Host /{ /^Host pocketsizefund-production$/d; /^Host /!d; }' "$SSH_CONFIG" || true
    sed -i.bak '${ /^Host /!d; }' "$SSH_CONFIG" || true
fi

rm -f swarm.pem

echo "Removing Docker contexts"
docker context use default >/dev/null >&1 || true
for context in pocketsizefund-production pocketsizefund-local; do
    docker context rm -f "$context" >/dev/null >&1 || true
done

echo "Infrastructure taken down successfully"
```

### applications

> Build and deploy application containers

#### build

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

> Deploy applications to both local and production Docker swarm

```bash
set -euo pipefail

echo "Deploying applications"

if docker context ls --format '{{.Name}}' | grep -q "^pocketsizefund-production$"; then
    echo "Deploying to production"
    docker context use pocketsizefund-production
    docker stack deploy -c applications/stack.yml applications --with-registry-auth
else
    echo "️Production context not found, skipping production deployment"
fi

if docker context ls --format '{{.Name}}' | grep -q "^pocketsizefund-local$"; then
    echo "Deploying to local"
    docker context use pocketsizefund-local
    docker stack deploy -c applications/stack.yml applications --with-registry-auth
else
    echo "️Local context not found, skipping local deployment"
fi

docker context use default >/dev/null >&1 || true

echo "Application deployment completed"
```

## test

> Test application endpoints and service health across environments

### endpoints

> Test HTTP endpoints for DataManager and PortfolioManager services

```bash
set -euo pipefail

echo "Testing application endpoints"

test_endpoint() {
    local name="1$"
    local url="2$"
    local context="3$"

    printf "%-25s %-15s " "$name" "[$context]"

    if timeout 10 curl -sf "$url" >/dev/null 2>&1; then
        echo " OK"
        return 0
    elif timeout 10 curl -s "$url" >/dev/null 2>&1; then
        echo " RESPONDING (non-200)"
        return 1
    else
        echo " FAILED"
        return 1
    fi
}

cd infrastructure
MANAGER_IP=""
if pulumi stack --show-name >/dev/null 2>&1; then
    MANAGER_IP=$(pulumi stack output managerIp 2>/dev/null || echo "")
fi
cd ..

echo "Local endpoints:"
test_endpoint "DataManager" "http://localhost:8080/health" "local"
test_endpoint "DataManager (root)" "http://localhost:8080/" "local"
test_endpoint "PortfolioManager" "http://localhost:8081/health" "local"
test_endpoint "PortfolioManager (root)" "http://localhost:8081/" "local"

if [[ -n "$MANAGER_IP" ]]; then
    echo ""
    echo "Production endpoints (IP: $MANAGER_IP):"
    test_endpoint "DataManager" "http://$MANAGER_IP:8080/health" "production"
    test_endpoint "DataManager (root)" "http://$MANAGER_IP:8080/" "production"
    test_endpoint "PortfolioManager" "http://$MANAGER_IP:8081/health" "production"
    test_endpoint "PortfolioManager (root)" "http://$MANAGER_IP:8081/" "production"
else
    echo "️Production manager IP not available - skipping production tests"
fi

docker context use default >/dev/null >&1 || true
echo " Endpoint testing completed"
```

### health

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

> Run complete test suite (endpoints + health checks)

```bash
set -euo pipefail

echo "Running complete test suite"

mask test endpoints
mask test health

echo "Complete test suite finished"
```

## docker

> Docker context and service management commands

### images

> Manage Docker images for applications

#### build (application_name) (stage_name)

> Build application Docker images locally

```bash
set -euo pipefail

echo "Building application image locally"

set -a

source "$MASKFILE_DIR/.env"

set +a

docker build --platform linux/amd64 --target runner -f applications/$application_name/Dockerfile -t pocketsizefund/$application_name_$stage_name:latest -t $AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com/pocketsizefund/$application_name_$stage_name:latest .

echo "Application image built: $application_name"
```

#### push (application_name) (stage_name)

> Push application Docker image to ECR

```bash
set -euo pipefail

echo "Pushing application image to ECR"

set -a

source "$MASKFILE_DIR/.env"

set +a

aws ecr get-login-password --region us-east-1 --profile $AWS_PROFILE | docker login --username AWS --password-stdin $AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com

docker push $AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com/pocketsizefund/$application_name_$stage_name:latest

echo "Application image pushed: $application_name"
```

### context

> Switch Docker context between local and production environments

#### local

> Switch to local Docker swarm context

```bash
docker context use pocketsizefund-local
echo "Switched to local context"
docker context ls | grep "\*"
```

#### production

> Switch to production Docker swarm context

```bash
docker context use pocketsizefund-production
echo "Switched to production context"
docker context ls | grep "\*"
```

#### default

> Switch back to default Docker context

```bash
docker context use default
echo "Switched to default context"
```

### services

> Docker swarm service management

#### ls

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

> Docker stack operations for infrastructure and applications

#### ls

> List all deployed stacks

```bash
echo "Deployed Docker stacks:"
docker stack ls
```

#### ps

> Show tasks for infrastructure and application stacks

```bash
echo "️Infrastructure stack:"
docker stack ps infrastructure >/dev/null || echo "Infrastructure stack not deployed"
echo "Applications stack:"
docker stack ps applications >/dev/null || echo "Applications stack not deployed"
```

#### rm

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

> Show comprehensive system status across all environments

```bash
set -euo pipefail

echo "System status"

echo "Docker contexts:"
docker context ls

echo "️Infrastructure status:"
cd infrastructure
if pulumi stack --show-name >/dev/null; then
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

> Manage Docker Swarm secrets for application configuration

### list

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

> Python development tools and code quality checks

### rust

> Rust development workflow commands

#### check

> Check Rust compilation

```bash
set -euo pipefail

echo "Check Rust compilation"
cargo check 
echo "Rust compiled successfully"
```

#### format

> Format Rust code

```bash
set -euo pipefail

echo "Formatting Rust code"
cargo fmt --all
echo "Rust code formatted successfully"
```

#### lint

> Run Rust code quality checks

```bash
set -euo pipefail

echo "Running Rust lint checks"
cargo clippy
echo "Rust linting completed successfully"
```

#### test

> Run Rust tests

```bash
set -euo pipefail

echo "Running Rust tests"
cargo test --workspace --verbose
echo "Rust tests completed successfully"
```

#### all

> Full Rust development checks

```bash
set -euo pipefail

echo "Running Rust development checks"

mask development rust check

mask development rust format

mask development rust lint

mask development rust test

echo "Rust development workflow completed successfully"
```

### python

> Python development workflow commands

#### install

> Install Python dependencies

```bash
set -euo pipefail

echo "Installing Python dependencies"
export COMPOSE_BAKE=true
uv sync --all-packages --all-groups

echo "Python dependencies installed successfully"
```

#### format

> Format Python code

```bash
set -euo pipefail

echo "Formatting Python code"
ruff format

echo "Python code formatted successfully"
```

#### dead-code

> Check for dead Python code

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

> Run comprehensive Python code quality checks

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

> Run Python tests using Docker Compose with coverage reporting

```bash
set -euo pipefail

echo "Running Python tests"

uv run coverage run --parallel-mode -m pytest && uv run coverage combine && uv run coverage report && uv run coverage xml -o coverage_output/.python_coverage.xml

echo "Python tests completed successfully"
```

#### all

> Full Python development checks

```bash
set -euo pipefail

echo "Running Python development checks"

mask development python install

mask development python format

mask development python lint

mask development python dead-code

mask development python test

echo "Development workflow completed successfully"
```

## logs

> Quick access to service logs across environments

### infrastructure

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

## model

> Model management commands

### train [application_name]

> Train machine learning model

```bash
set -euo pipefail

set -a

source "$MASKFILE_DIR/.env"

set +a

aws ecr get-login-password --region us-east-1 --profile $AWS_PROFILE | docker login --username AWS --password-stdin $AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com

echo "Training model: $application_name"

uv run python applications/$application_name/src/$application_name/run_training_job.py
```

### artifact

#### download [application_name]

> Manage model artifacts

```python
import os
import dotenv
import boto3
import tarfile

print("Downloading model artifact")

dotenv.load_dotenv(os.getenv("MASKFILE_DIR") + '/.env')

session = boto3.Session(profile_name=os.getenv("AWS_PROFILE"))

s3_client = session.client('s3')

application_name = os.getenv("application_name")

print("Application name:", application_name)

artifacts_bucket = os.getenv("AWS_S3_ARTIFACTS_BUCKET_NAME")

file_objects = s3_client.list_objects_v2(
    Bucket=artifacts_bucket,
    Prefix=f"artifacts/{application_name}",
)

options = set()

for file_object in file_objects.get('Contents', []):
    file_object_name = file_object['Key']

    file_object_name_parts = file_object_name.split('/')

    options.add(file_object_name_parts[1])

print("Available artifacts:")
for option in options:
    print(f"{option}")

selected_option = input("Select an artifact to download: ")

print("Selected artifact:", selected_option)

target_path = f"artifacts/{selected_option}/output/model.tar.gz"
destination_directory = f"applications/{application_name}/src/{application_name}/"
destination_path = os.path.join(destination_directory, "model.tar.gz")

s3_client.download_file(
    Bucket=artifacts_bucket,
    Key=target_path,
    Filename=destination_path,
)

with tarfile.open(destination_path, 'r:gz') as tar:
    tar.extractall(path=destination_directory, filter="data")

print("Artifact downloaded and extracted successfully")
```
