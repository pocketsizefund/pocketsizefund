FROM golang:1.22-alpine

ENV PATH="/root/.pulumi/bin:${PATH}"
ARG PULUMI_ACCESS_TOKEN

WORKDIR /app

COPY go.mod go.sum ./

RUN apk add --no-cache git curl && \
    curl -fsSL https://get.pulumi.com | sh && \
    go mod download

COPY . .

# RUN pulumi up --yes --stack pocketsizefund/development
RUN pulumi plugin install --exact
