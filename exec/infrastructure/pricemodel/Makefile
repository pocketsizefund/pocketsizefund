IMAGE=pocketsizefund/price-model
PLATFORM=linux/amd64

build:
    docker build -t $(IMAGE):latest -t $(IMAGE):$$(poetry version -s) --platform $(PLATFORM) .

run:
    docker run \
	--platform $(PLATFORM) \
	-p 8080:8080 \
	-e DARQUBE_API_KEY=$(DARQUBE_API_KEY) \
	-e ALPACA_API_KEY=$(ALPACA_API_KEY) \
	-e ALPACA_API_SECRET=$(ALPACA_API_SECRET) \
	-e ALPHA_VANTAGE_API_KEY=$(ALPHA_VANTAGE_API_KEY) \
	-e EDGAR_USER_AGENT=$(EDGAR_USER_AGENT) \
	-e MODEL_FILE_NAME=$(MODEL_FILE_NAME) \
	$(IMAGE):latest

push:
	docker push $(IMAGE):latest
	docker push $(IMAGE):$$(poetry version -s)
