.PHONY: setup pipeline-test

PIPELINE_REQUIREMENTS=pipelines.requirements
PIPELINE_VENV=.psf-pipelines
	
setup:
	brew install uv


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

