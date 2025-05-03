# Consider adding a non-root user for production

For improved security, consider creating and using a non-root user in the production stage. This follows the principle of least privilege and is a container security best practice.

```diff
FROM debian:bullseye-slim AS production

RUN apt-get update && apt-get install -y libgmp10 ca-certificates && rm -rf /var/lib/apt/lists/*

+# Create a non-root user to run the application
+RUN groupadd -r platform && useradd -r -g platform platform

COPY --from=builder /app/platform-exe /usr/local/bin/app

+# Change ownership of the app binary
+RUN chown platform:platform /usr/local/bin/app

+# Switch to non-root user
+USER platform

ENTRYPOINT ["/usr/local/bin/app"]
```
