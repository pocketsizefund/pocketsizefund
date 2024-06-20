IMAGE=pocketsizefund/position-manager

build:
	docker build -t $(IMAGE):latest -t $(IMAGE):$$(poetry version -s) --platform linux/amd64 .

run:
	docker run --platform linux/amd64 $(IMAGE):latest

push:
	docker push $(IMAGE):latest
	docker push $(IMAGE):$$(poetry version -s)
