.PHONY: setup pipeline-test

PIPELINE_REQUIREMENTS=pipelines.requirements
PIPELINE_VENV=.psf-pipelines
	
setup:
	curl -sSL https://install.python-poetry.org | python3 -
	curl -LsSf https://astral.sh/uv/install.sh | sh
	brew install pre-commit
	pre-commit

sync-requirements:
	uv venv $(PIPELINE_VENV) && \
		source $(PIPELINE_VENV)/bin/activate && \
		uv pip compile -q -o $(PIPELINE_REQUIREMENTS).txt $(PIPELINE_REQUIREMENTS).in && \
		uv pip compile -q -o $(PIPELINE_REQUIREMENTS).dev.txt $(PIPELINE_REQUIREMENTS).dev.in && \
		deactivate
	rm -rf $(PIPELINE_VENV)


virtualenv-pipelines:
	uv venv $(PIPELINE_VENV) && \
		source $(PIPELINE_VENV)/bin/activate && \
		uv pip install -q -r pipelines.requirements.txt && \
		uv pip install -q -r pipelines.requirements.dev.txt
	
test-pipelines:
	uv venv $(PIPELINE_VENV) && \
		source $(PIPELINE_VENV)/bin/activate && \
		uv pip install -q -r pipelines.requirements.txt && \
		uv pip install -q -r pipelines.requirements.dev.txt && \
		coverage run -m pytest && \
		coverage report -m && \
		$(PIPELINE_VENV)/bin/deactivate
	rm -rf $(PIPELINE_VENV)

deploy-pipelines:
	uv venv $(PIPELINE_VENV) && \
		source $(PIPELINE_VENV)/bin/activate && \
		uv pip install -q -r pipelines.requirements.txt && \
		python -m pipelines.deploy && \
		$(PIPELINE_VENV)/bin/deactivate
	rm -rf $(PIPELINE_VENV)

lint:
	poetry run ruff check .

lint-dead:
	poetry run vulture .

test: 
	poetry run coverage run --module --parallel-mode unittest discover pkg/data
	poetry run coverage run --module --parallel-mode unittest discover pkg/trade
	poetry run coverage run --module --parallel-mode unittest discover pkg/storage
	poetry run coverage combine
	poetry run coverage report
	poetry run coverage xml --omit='pkg/*/test_*.py' --include='pkg/*'
	rm -rf .coverage/
