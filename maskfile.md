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

gh auth status >/dev/null 2>&1 || gh auth login

echo "GitHub CLI configuration completed"

echo "Development environment setup completed successfully"
```

## infrastructure

> Manage infrastructure resources 

### images

> Manage Docker images for applications

#### build (application_name) (stage_name)

> Build application Docker images

```bash
set -euo pipefail

echo "Building application image locally"

aws_account_id=$(aws sts get-caller-identity --query Account --output text)

docker build \
    --platform linux/amd64 \
    --target ${stage_name} \
    --file applications/${application_name}/Dockerfile \
    --tag pocketsizefund/${application_name}-${stage_name}:latest \
    --tag ${aws_account_id}.dkr.ecr.us-east-1.amazonaws.com/pocketsizefund/${application_name}-${stage_name}:latest \
    .

echo "Application image built: ${application_name} ${stage_name}"
```

#### push (application_name) (stage_name)

> Push application Docker image to ECR

```bash
set -euo pipefail

echo "Pushing application image to ECR"

aws_account_id=$(aws sts get-caller-identity --query Account --output text)

aws ecr get-login-password 
    --region us-east-1 | docker login 
    --username AWS 
    --password-stdin ${aws_account_id}.dkr.ecr.us-east-1.amazonaws.com

docker push ${aws_account_id}.dkr.ecr.us-east-1.amazonaws.com/pocketsizefund/${application_name}-${stage_name}:latest

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

pulumi up --diff --yes --stack production

echo "Forcing ECS service deployments to pull latest images"

CLUSTER=$(pulumi stack output aws_ecs_cluster_name --stack production 2>/dev/null || echo "")

if [ -z "$CLUSTER" ]; then
    echo "Cluster not found - skipping service deployments (initial setup)"
else
    for SERVICE in pocketsizefund-datamanager pocketsizefund-portfoliomanager pocketsizefund-equitypricemodel; do
        echo "Checking if $SERVICE exists and is ready"

        # Wait up to 60 seconds for service to be active
        RETRY_COUNT=0
        MAX_RETRIES=12
        SERVICE_READY=false

        while [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
            SERVICE_STATUS=$(aws ecs describe-services \
                --cluster "$CLUSTER" \
                --services "$SERVICE" \
                --query 'services[0].status' \
                --output text 2>/dev/null || echo "NONE")

            if [ "$SERVICE_STATUS" = "ACTIVE" ]; then
                SERVICE_READY=true
                break
            elif [ "$SERVICE_STATUS" = "NONE" ]; then
                echo "Service not found, waiting ($((RETRY_COUNT + 1))/$MAX_RETRIES)"
            else
                echo "Service status: $SERVICE_STATUS, waiting ($((RETRY_COUNT + 1))/$MAX_RETRIES)"
            fi

            sleep 5
            RETRY_COUNT=$((RETRY_COUNT + 1))
        done

        if [ "$SERVICE_READY" = true ]; then
            echo "Forcing new deployment for $SERVICE"
            aws ecs update-service \
                --cluster "$CLUSTER" \
                --service "$SERVICE" \
                --force-new-deployment \
                --no-cli-pager \
                --output text > /dev/null 2>&1 && echo "Deployment initiated" || echo "Failed to force deployment"
        else
            echo "Skipping $SERVICE (not ready after 60s - may be initial deployment)"
        fi
    done

    echo "Stack update complete - ECS is performing rolling deployments"
    echo "Monitor progress: aws ecs describe-services --cluster $CLUSTER --services pocketsizefund-portfoliomanager"
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

BASE_URL=$(pulumi stack output psf_base_url --stack production 2>/dev/null || echo "")

if [ -z "$BASE_URL" ]; then
    echo "Error: psf_base_url not found - infrastructure might not be deployed"
    exit 1
fi

case "$application_name" in
    portfoliomanager)
        FULL_URL="${BASE_URL}/portfolio"
        echo "Creating portfolio: $FULL_URL"

        curl -X POST "$FULL_URL" \
            -H "Content-Type: application/json" \
            -w "\nHTTP Status: %{http_code}\n" \
            -s
        ;;

    datamanager)
        if [ -n "${date_range:-}" ]; then
            cd "${MASKFILE_DIR}"
            uv run python tools/sync_equity_bars_data.py "$BASE_URL" "$date_range"
        else
            CURRENT_DATE=$(date -u +"%Y-%m-%dT00:00:00Z")
            FULL_URL="${BASE_URL}/equity-bars"
            echo "Syncing equity bars: $FULL_URL"

            curl -X POST "$FULL_URL" \
                -H "Content-Type: application/json" \
                -d "{\"date\": \"$CURRENT_DATE\"}" \
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

echo "Python development checks completed successfully"
```

## models

> Model management commands

### train (application_name)

> Train machine learning model

```bash
set -euo pipefail

# Load environment variables in isolated subshell
(
    set -a
    source "${MASKFILE_DIR}/.env"
    set +a

    export APPLICATION_NAME="${application_name}"

    uv run python tools/run_training_job.py
)
```

### artifacts

#### download (application_name)

> Manage model artifacts

```bash
set -euo pipefail

# Load environment variables in isolated subshell
(
    set -a
    source "${MASKFILE_DIR}/.env"
    set +a

    export APPLICATION_NAME="${application_name}"

    uv run python tools/download_model_artifacts.py
)
```


