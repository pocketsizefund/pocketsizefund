
```sh
$ kn event send --to serving.knative.dev:v1:discord-service-00001 \
  --type com.example.test \
  --source knative-cli \
  --id test-event-1 \
  --field data.message=Hello,Knative! \
  --namespace live
```