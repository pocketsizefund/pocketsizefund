setup:
	curl -sSL https://install.python-poetry.org | python3 -
	curl -LsSf https://astral.sh/uv/install.sh | sh
	brew install pre-commit
	pre-commit

lint:
	poetry run ruff check --output-format=github .

dead-code:
	poetry run vulture --min-confidence 80 .

unit-test:
	poetry run coverage run --module --parallel-mode unittest discover pkg/data
	poetry run coverage run --module --parallel-mode unittest discover pkg/trade
	poetry run coverage run --module --parallel-mode unittest discover pkg/storage
	poetry run coverage combine
	poetry run coverage report
	poetry run coverage xml --omit='pkg/*/test_*.py' --include='pkg/*'
	rm -rf .coverage/
	rm -rf .coverage/
