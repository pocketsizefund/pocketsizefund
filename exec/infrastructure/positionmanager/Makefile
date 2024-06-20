IMAGE=pocketsizefund/position-manager

build:
	docker build -t $(IMAGE):latest -t $(IMAGE):$$(poetry version -s) --platform linux/arm64 .

run:
	docker run --platform linux/arm64 $(IMAGE):latest

push:
	docker push $(IMAGE):latest
	docker push $(IMAGE):$$(poetry version -s)
