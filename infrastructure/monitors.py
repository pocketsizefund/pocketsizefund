import pulumi
import pulumi_aws as aws
import pulumi_eks as eks
from tags import common_tags


def create_prometheus_scraper(
    workspace_arn: pulumi.Output[str],
    cluster: eks.Cluster,
) -> aws.amp.Scraper:
    return aws.amp.Scraper(
        resource_name="pocketsizefund-prometheus-scraper",
        alias="pocketsizefund-cluster-scraper",
        scrape_configuration=cluster.eks_cluster.endpoint.apply(
            lambda endpoint: f"""
global:
  scrape_interval: 15m
  evaluation_interval: 15m
scrape_configs:
- job_name: 'kubernetes-apiservers'
  kubernetes_sd_configs:
  - role: endpoints
    api_server: {endpoint}
  scheme: https
  tls_config:
    ca_file: /var/run/secrets/kubernetes.io/serviceaccount/ca.crt
  bearer_token_file: /var/run/secrets/kubernetes.io/serviceaccount/token
  relabel_configs:
  - source_labels: [__meta_kubernetes_namespace, __meta_kubernetes_service_name, __meta_kubernetes_endpoint_port_name]
    action: keep
    regex: default;kubernetes;https
- job_name: 'kubernetes-nodes'
  kubernetes_sd_configs:
  - role: node
    api_server: {endpoint}
  scheme: https
  tls_config:
    ca_file: /var/run/secrets/kubernetes.io/serviceaccount/ca.crt
  bearer_token_file: /var/run/secrets/kubernetes.io/serviceaccount/token
  relabel_configs:
  - action: labelmap
    regex: __meta_kubernetes_node_label_(.+)
  - target_label: __address__
    replacement: kubernetes.default.svc:443
  - source_labels: [__meta_kubernetes_node_name]
    regex: (.+)
    target_label: __metrics_path__
    replacement: /api/v1/nodes/$1/proxy/metrics
- job_name: 'kubernetes-pods'
  kubernetes_sd_configs:
  - role: pod
    api_server: {endpoint}
  relabel_configs:
  - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
    action: keep
    regex: true
  - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_path]
    action: replace
    target_label: __metrics_path__
    regex: (.+)
  - source_labels: [__address__, __meta_kubernetes_pod_annotation_prometheus_io_port]
    action: replace
    regex: ([^:]+)(?::\\d+)?;(\\d+)
    replacement: $1:$2
    target_label: __address__
  - action: labelmap
    regex: __meta_kubernetes_pod_label_(.+)
  - source_labels: [__meta_kubernetes_namespace]
    action: replace
    target_label: kubernetes_namespace
  - source_labels: [__meta_kubernetes_pod_name]
    action: replace
    target_label: kubernetes_pod_name
"""  # noqa: E501
        ),
        destination=aws.amp.ScraperDestinationArgs(
            amp=aws.amp.ScraperDestinationAmpArgs(
                workspace_arn=workspace_arn,
            ),
        ),
        source=aws.amp.ScraperSourceArgs(
            eks=aws.amp.ScraperSourceEksArgs(
                cluster_arn=cluster.eks_cluster.arn,
                subnet_ids=cluster.private_subnet_ids,  # type: ignore
            ),
        ),
        tags=common_tags,
    )
