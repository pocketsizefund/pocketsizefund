setup:
	curl -sSL https://install.python-poetry.org | python3 -
	curl -LsSf https://astral.sh/uv/install.sh | sh
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
	brew install kubectl
	brew install minikube
	curl -L https://github.com/a8m/envsubst/releases/download/v1.2.0/envsubst-`uname -s`-`uname -m` -o envsubst
	poetry install

lint:
	poetry run ruff check --output-format=github .

dead-code:
	poetry run vulture --min-confidence 80 .

unit-test:
	poetry run coverage run --parallel-mode --module pytest pkg/data
	poetry run coverage run --parallel-mode --module pytest pkg/trade
	poetry run coverage run --parallel-mode --module pytest pkg/storage
	poetry run coverage combine
	poetry run coverage report
	poetry run coverage xml --omit='pkg/*/test_*.py' --include='pkg/*'
	rm -rf .coverage/
