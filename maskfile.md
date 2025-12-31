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

echo "Prerequisites check completed"

echo "Configuring GitHub CLI"

if ! gh auth status >/dev/null 2>&1; then
    echo "GitHub CLI not authenticated"
    echo "Run 'gh auth login' before setup"
    exit 1
fi

echo "GitHub CLI configuration completed"

echo "Development environment setup completed successfully"
```

## infrastructure

> Manage infrastructure resources 

### images

> Manage Docker images for applications

#### build (application_name) (stage_name)

> Build application Docker images with optional cache pull

```bash
set -euo pipefail

echo "Building application image"

aws_account_id=$(aws sts get-caller-identity --query Account --output text)
aws_region=${AWS_REGION}
if [ -z "$aws_region" ]; then
    echo "Error: AWS_REGION environment variable is not set"
    exit 1
fi

image_reference="${aws_account_id}.dkr.ecr.${aws_region}.amazonaws.com/pocketsizefund/${application_name}-${stage_name}"
cache_reference="${image_reference}:buildcache"

echo "Setting up Docker Buildx"
docker buildx create --use --name psf-builder 2>/dev/null || docker buildx use psf-builder || (echo "Using default buildx builder" && docker buildx use default)

echo "Logging into ECR (to pull cache if available)"
aws ecr get-login-password --region ${aws_region} | docker login \
    --username AWS \
    --password-stdin ${aws_account_id}.dkr.ecr.${aws_region}.amazonaws.com 2>/dev/null || echo "Could not authenticate to ECR for cache (will build without cache)"

echo "Building with caching (will continue if cache doesn't exist)"
docker buildx build \
    --platform linux/amd64 \
    --target ${stage_name} \
    --file applications/${application_name}/Dockerfile \
    --tag ${image_reference}:latest \
    --cache-from type=registry,ref=${cache_reference} \
    --cache-to type=registry,ref=${cache_reference},mode=max \
    --load \
    .

echo "Application image built locally: ${application_name} ${stage_name}"
```

#### push (application_name) (stage_name)

> Push application Docker image to ECR

```bash
set -euo pipefail

echo "Pushing application image to ECR"

aws_account_id=$(aws sts get-caller-identity --query Account --output text)
aws_region=${AWS_REGION}
if [ -z "$aws_region" ]; then
    echo "Error: AWS_REGION environment variable is not set"
    exit 1
fi

image_reference="${aws_account_id}.dkr.ecr.${aws_region}.amazonaws.com/pocketsizefund/${application_name}-${stage_name}"

echo "Logging into ECR"
aws ecr get-login-password --region ${aws_region} | docker login \
    --username AWS \
    --password-stdin ${aws_account_id}.dkr.ecr.${aws_region}.amazonaws.com

echo "Pushing image"
docker push ${image_reference}:latest

echo "Application image pushed: ${application_name} ${stage_name}"
```

### stack

> Manage infrastructure stack

#### up

> Launch or update infrastructure stack

```bash
set -euo pipefail

cd infrastructure/

echo "Launching infrastructure"

organization_name=$(pulumi whoami)

if [ -z "${organization_name}" ]; then
    echo "Unable to determine Pulumi organization name - ensure you are logged in"
    exit 1
fi

pulumi stack select ${organization_name}/pocketsizefund/production --create

pulumi up --diff --yes

echo "Forcing ECS service deployments to pull latest images"

cluster=$(pulumi stack output aws_ecs_cluster_name --stack production 2>/dev/null || echo "")

if [ -z "$cluster" ]; then
    echo "Cluster not found - skipping service deployments (initial setup)"
else
    for service in pocketsizefund-datamanager pocketsizefund-portfoliomanager pocketsizefund-equitypricemodel; do
        echo "Checking if $service exists and is ready"

        # Wait up to 60 seconds for service to be active
        retry_count=0
        maximum_retries=12
        retry_wait_seconds=5
        service_is_ready=false

        while [ $retry_count -lt $maximum_retries ]; do
            service_status=$(aws ecs describe-services \
                --cluster "$cluster" \
                --services "$service" \
                --query 'services[0].status' \
                --output text 2>/dev/null || echo "NONE")

            if [ "$service_status" = "ACTIVE" ]; then
                service_is_ready=true
                echo "Service $service is ACTIVE"
                break
            elif [ "$service_status" = "NONE" ]; then
                echo "Service not found, waiting ($((retry_count + 1))/$maximum_retries)"
            else
                echo "Service status: $service_status, waiting ($((retry_count + 1))/$maximum_retries)"
            fi

            sleep $retry_wait_seconds
            retry_count=$((retry_count + 1))
        done

        if [ "$service_is_ready" = true ]; then
            echo "Forcing new deployment for $service"
            aws ecs update-service \
                --cluster "$cluster" \
                --service "$service" \
                --force-new-deployment \
                --no-cli-pager \
                --output text > /dev/null 2>&1 && echo "Deployment initiated" || echo "Failed to force deployment"
        else
            echo "Skipping $service (not ready after 60s - may be initial deployment)"
        fi
    done

    echo "Stack update complete - ECS is performing rolling deployments"
    echo "Monitor progress: aws ecs describe-services --cluster $cluster --services pocketsizefund-portfoliomanager"
fi

echo "Infrastructure launched successfully"
```

#### down

> Teardown infrastructure stack

```bash
set -euo pipefail

echo "Tearing down infrastructure"

cd infrastructure/

pulumi down --yes --stack production

echo "Infrastructure torn down successfully"
```

### services

> Manage infrastructure services

#### invoke (application_name) [date_range]

> Invoke service REST endpoint

```bash
set -euo pipefail

echo "Invoking ${application_name} service"

cd infrastructure/

base_url=$(pulumi stack output psf_base_url --stack production 2>/dev/null || echo "")

if [ -z "$base_url" ]; then
    echo "psf_base_url not found - infrastructure might not be deployed"
    exit 1
fi

case "$application_name" in
    portfoliomanager)
        full_url="${base_url}/portfolio"
        echo "Creating portfolio: $full_url"

        curl -X POST "$full_url" \
            -H "Content-Type: application/json" \
            -w "\nHTTP Status: %{http_code}\n" \
            -s
        ;;

    datamanager)
        if [ -n "${date_range:-}" ]; then
            cd "${MASKFILE_DIR}"
            uv run python tools/sync_equity_bars_data.py "$base_url" "$date_range"
        else
            current_date=$(date -u +"%Y-%m-%dT00:00:00Z")
            full_url="${base_url}/equity-bars"
            echo "Syncing equity bars: $full_url"

            curl -X POST "$full_url" \
                -H "Content-Type: application/json" \
                -d "{\"date\": \"$current_date\"}" \
                -w "\nHTTP Status: %{http_code}\n" \
                -s
        fi
        ;;

    *)
        echo "Unknown application name: ${application_name}"
        echo "Valid options: portfoliomanager, datamanager"
        exit 1
        ;;
esac
```

## development

> Python development tools and code quality checks

### rust

> Rust development workflow commands

#### update

> Update Rust dependencies

```bash
set -euo pipefail

echo "Updating Rust dependencies"

cargo update

echo "Rust dependencies updated successfully"
```

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

mask development rust update

mask development rust check

mask development rust format

mask development rust lint

mask development rust test

echo "Rust development checks completed successfully"
```

### python

> Python development workflow commands

#### install

> Install Python dependencies

```bash
set -euo pipefail

echo "Installing Python dependencies"

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

echo "Running dead code analysis"

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

ruff check \
    --output-format=github \
    .

echo "Python linting completed successfully"
```

#### test

> Run Python tests using coverage reporting

```bash
set -euo pipefail

echo "Running Python tests"

uv run coverage run --parallel-mode -m pytest && uv run coverage combine && uv run coverage report && uv run coverage xml -o coverage/.python.xml

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

echo "Python development checks completed successfully"
```

## models

> Model management commands

### train (application_name)

> Train machine learning model

```bash
set -euo pipefail

export APPLICATION_NAME="${application_name}"

uv run python tools/run_training_job.py
```

### artifacts

#### download (application_name)

> Manage model artifacts

```bash
set -euo pipefail

export APPLICATION_NAME="${application_name}"

uv run python tools/download_model_artifacts.py
```

