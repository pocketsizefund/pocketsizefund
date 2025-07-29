import pulumi
import pulumi_aws as aws
import pulumi_eks as eks
from tags import pulumi_tags


def create_prometheus_scraper(
    prometheus_workspace_arn: pulumi.Output[str],
    kubernetes_cluster: eks.Cluster,
    security_group: aws.ec2.SecurityGroup,
) -> aws.amp.Scraper:
    scrape_configuration = pulumi.Output.json_dumps(
        {
            "global": {
                "scrape_interval": "15m",
                "evaluation_interval": "15m",
            },
            "scrape_configs": [
                {
                    "job_name": "kubernetes-apiservers",
                    "kubernetes_sd_configs": [
                        {
                            "role": "endpoints",
                            "api_server": kubernetes_cluster.core.endpoint,
                        }
                    ],
                    "scheme": "https",
                    "tls_config": {
                        "ca_file": "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",  # noqa: E501
                    },
                    "bearer_token_file": "/var/run/secrets/kubernetes.io/serviceaccount/token",  # noqa: E501
                    "relabel_configs": [
                        {
                            "source_labels": [
                                "__meta_kubernetes_namespace",
                                "__meta_kubernetes_service_name",
                                "__meta_kubernetes_endpoint_port_name",
                            ],
                            "action": "keep",
                            "regex": "default;kubernetes;https",
                        }
                    ],
                },
                {
                    "job_name": "kubernetes-nodes",
                    "kubernetes_sd_configs": [
                        {
                            "role": "node",
                            "api_server": kubernetes_cluster.core.endpoint,
                        }
                    ],
                    "scheme": "https",
                    "tls_config": {
                        "ca_file": "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",  # noqa: E501
                    },
                    "bearer_token_file": "/var/run/secrets/kubernetes.io/serviceaccount/token",  # noqa: E501
                    "relabel_configs": [
                        {
                            "action": "labelmap",
                            "regex": "__meta_kubernetes_node_label_(.+)",
                        },
                        {
                            "target_label": "__address__",
                            "replacement": "kubernetes.default.svc:443",
                        },
                        {
                            "source_labels": ["__meta_kubernetes_node_name"],
                            "regex": "(.+)",
                            "target_label": "__metrics_path__",
                            "replacement": "/api/v1/nodes/$1/proxy/metrics",
                        },
                    ],
                },
                {
                    "job_name": "kubernetes-pods",
                    "kubernetes_sd_configs": [
                        {
                            "role": "pod",
                            "api_server": kubernetes_cluster.core.endpoint,
                        }
                    ],
                    "relabel_configs": [
                        {
                            "source_labels": [
                                "__meta_kubernetes_pod_annotation_prometheus_io_scrape"
                            ],
                            "action": "keep",
                            "regex": "true",
                        },
                        {
                            "source_labels": [
                                "__meta_kubernetes_pod_annotation_prometheus_io_path"
                            ],
                            "action": "replace",
                            "target_label": "__metrics_path__",
                            "regex": "(.+)",
                        },
                        {
                            "source_labels": [
                                "__address__",
                                "__meta_kubernetes_pod_annotation_prometheus_io_port",
                            ],
                            "action": "replace",
                            "regex": r"([^:]+)(?::\\d+)?;(\\d+)",
                            "replacement": "$1:$2",
                            "target_label": "__address__",
                        },
                        {
                            "action": "labelmap",
                            "regex": "__meta_kubernetes_pod_label_(.+)",
                        },
                        {
                            "source_labels": ["__meta_kubernetes_namespace"],
                            "action": "replace",
                            "target_label": "kubernetes_namespace",
                        },
                        {
                            "source_labels": ["__meta_kubernetes_pod_name"],
                            "action": "replace",
                            "target_label": "kubernetes_pod_name",
                        },
                    ],
                },
            ],
        }
    )

    return aws.amp.Scraper(
        resource_name="pocketsizefund-prometheus-scraper",
        alias="pocketsizefund-cluster-scraper",
        scrape_configuration=scrape_configuration,
        destination=aws.amp.ScraperDestinationArgs(
            amp=aws.amp.ScraperDestinationAmpArgs(
                workspace_arn=prometheus_workspace_arn,
            ),
        ),
        source=aws.amp.ScraperSourceArgs(
            eks=aws.amp.ScraperSourceEksArgs(
                cluster_arn=kubernetes_cluster.eks_cluster.arn,
                subnet_ids=kubernetes_cluster.eks_cluster.vpc_config.subnet_ids,
                security_group_ids=[security_group.id],
            ),
        ),
        opts=pulumi.ResourceOptions(
            depends_on=[
                kubernetes_cluster,
                security_group,
            ],
        ),
        tags=pulumi_tags,
    )
