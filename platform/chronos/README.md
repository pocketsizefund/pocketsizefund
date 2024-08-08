

## Triggering the Service

Can run ping it locally:
```sh
http POST http://127.0.0.1:8080/market-status \
    Content-Type:application/cloudevents+json \
    ce-specversion:1.0 \
    message="Good morning" \
    specversion="1.0" \
    id="1" \
    type="dev.knative.example" \
    source="platform:chronos"
```

Pinging it with the `kn` CLI:

```sh
kn event send \
    --to Service:serving.knative.dev/v1:chronos-00001 \
--namespace live
```