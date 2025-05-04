# Consider adding resource limits

To prevent container resource starvation, consider adding memory and CPU limits.

```diff
    platform: linux/amd64
    container_name: platform
    ports:
      - "8080:8080"
    restart: unless-stopped
+    deploy:
+      resources:
+        limits:
+          cpus: '1.0'
+          memory: 1G
```
