setup:
	curl -sSL https://install.python-poetry.org | python3 -
	curl -LsSf https://astral.sh/uv/install.sh | sh
	curl -LO https://dl.k8s.io/release/v1.29.2/bin/darwin/arm64/kubectl
	sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl
	curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-darwin-arm64
	sudo install minikube-darwin-arm64 /usr/local/bin/minikube
	curl -L https://github.com/a8m/envsubst/releases/download/v1.2.0/envsubst-`uname -s`-`uname -m` -o envsubst
	poetry install --with dev,test

python-lint:
	poetry run ruff check --output-format=github .

python-dead-code:
	poetry run vulture --min-confidence 80 .

python-unit-test:
	poetry run coverage run --parallel-mode --module pytest pkg/data
	poetry run coverage run --parallel-mode --module pytest pkg/trade
	poetry run coverage run --parallel-mode --module pytest pkg/storage
	poetry run coverage combine
	poetry run coverage report
	poetry run coverage xml --omit='pkg/*/test_*.py' --include='pkg/*'
	rm -rf .coverage/

rust-lint:
	cd edgar-feed && \
		cargo fmt --all --

rust-fmt:
	cd edgar-feed && cargo fmt --all --

rust-unit-test:
	cd edgar-feed && cargo test