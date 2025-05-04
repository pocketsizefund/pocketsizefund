The platform:push task in .mise.toml (lines 60–69) will fail in CI/CD without logging into Docker Hub—and a search shows no docker login in .github/workflows/. Please add an authentication step, for example:

In .mise.toml:

```diff
 [tasks."platform:push"]
 description = "push platform image to dockerhub"
 depends = ["platform:build"]
 run = """
+  docker login -u $DOCKER_USERNAME -p $DOCKER_PASSWORD
   TIMESTAMP=$(date +%Y%m%d)
   cd platform
   docker push pocketsizefund/platform:latest
   docker push pocketsizefund/platform:${TIMESTAMP}
 """
```

Or, if you prefer handling login in your GitHub Actions workflow, insert before the push step:

```yaml
- name: Log in to Docker Hub
  run: docker login -u ${{ secrets.DOCKER_USERNAME }} -p ${{ secrets.DOCKER_PASSWORD }}
```
