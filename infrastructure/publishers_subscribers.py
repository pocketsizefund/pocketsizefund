import pulumi
import pulumi_kubernetes as k8s


def create_knative_serving_core(
    kubernetes_provider: k8s.Provider,
) -> k8s.yaml.v2.ConfigGroup:
    knative_serving_namespace = k8s.core.v1.Namespace(
        resource_name="pocketsizefund-knative-serving-namespace",
        metadata={"name": "knative-serving"},
        opts=pulumi.ResourceOptions(provider=kubernetes_provider),
    )

    knative_serving_crds = k8s.yaml.v2.ConfigGroup(  # custom resource definition
        resource_name="pocketsizefund-knative-serving-crds",
        files=[
            "https://github.com/knative/serving/releases/download/knative-v1.12.0/serving-crds.yaml"
        ],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=[knative_serving_namespace],
        ),
    )

    knative_serving_core = k8s.yaml.v2.ConfigGroup(
        resource_name="pocketsizefund-knative-serving-core",
        files=[
            "https://github.com/knative/serving/releases/download/knative-v1.12.0/serving-core.yaml"
        ],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=[knative_serving_crds],
            custom_timeouts=pulumi.CustomTimeouts(
                create="15m",
                update="15m",
                delete="15m",
            ),
        ),
    )

    # NEW ADDITION
    k8s.core.v1.ConfigMap(
        resource_name="pocketsizefund-knative-configuration-network",
        metadata=k8s.meta.v1.ObjectMetaArgs(
            name="config-network",
            namespace="knative-serving",
        ),
        data={
            "autocreate-cluster-domain-claims": "false",
            "auto-tls": "false",
            "http-protocol": "Redirected",
        },
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=[knative_serving_core],
        ),
    )

    return knative_serving_core


def create_knative_eventing_core(
    kubernetes_provider: k8s.Provider,
) -> k8s.yaml.v2.ConfigGroup:
    knative_eventing_namespace = k8s.core.v1.Namespace(
        resource_name="pocketsizefund-eventing-namespace",
        metadata={"name": "knative-eventing"},
        opts=pulumi.ResourceOptions(provider=kubernetes_provider),
    )

    knative_eventing_crds = k8s.yaml.v2.ConfigGroup(
        resource_name="pocketsizefund-knative-eventing-crds",
        files=[
            "https://github.com/knative/eventing/releases/download/knative-v1.12.0/eventing-crds.yaml"
        ],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=[knative_eventing_namespace],
        ),
    )

    return k8s.yaml.v2.ConfigGroup(
        resource_name="pocketsizefund-knative-eventing-core",
        files=[
            "https://github.com/knative/eventing/releases/download/knative-v1.12.0/eventing-core.yaml"
        ],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=[knative_eventing_crds],
            custom_timeouts=pulumi.CustomTimeouts(
                create="15m",
                update="15m",
                delete="15m",
            ),
        ),
    )


def create_knative_service(
    kubernetes_provider: k8s.Provider,
    service_name: str,
    image_reference: pulumi.Output[str],
    environment_variables: pulumi.Output[dict[str, str]] | None = None,
    depends_on: list[pulumi.Resource] | None = None,
) -> k8s.yaml.v2.ConfigGroup:
    formatted_environment_variables = (
        environment_variables.apply(
            lambda env_vars: [
                {"name": key, "value": value} for key, value in env_vars.items()
            ]
        )
        if environment_variables
        else []
    )

    content = {
        "apiVersion": "serving.knative.dev/v1",
        "kind": "Service",
        "metadata": {"name": service_name, "namespace": "default"},
        "spec": {
            "template": {
                "metadata": {
                    "annotations": {
                        "prometheus.io/scrape": "true",
                        "prometheus.io/path": "/metrics",
                        "prometheus.io/port": "8080",
                    }
                },
                "spec": {
                    "containers": [
                        {
                            "image": image_reference,
                            "name": service_name,
                            "env": formatted_environment_variables,
                        }
                    ]
                },
            }
        },
    }

    return k8s.yaml.v2.ConfigGroup(
        resource_name=f"pocketsizefund-knative-service-{service_name}",
        objs=[content],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=depends_on,
        ),
    )


def create_knative_broker(
    kubernetes_provider: k8s.Provider,
    knative_eventing_core: k8s.yaml.v2.ConfigGroup,
) -> k8s.yaml.v2.ConfigGroup:
    content = {
        "apiVersion": "eventing.knative.dev/v1",
        "kind": "Broker",
        "metadata": {
            "name": "default",
            "namespace": "default",
        },
    }

    return k8s.yaml.v2.ConfigGroup(
        resource_name="pocketsizefund-default-broker",
        objs=[content],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider, depends_on=[knative_eventing_core]
        ),
    )


def create_knative_trigger(
    kubernetes_provider: k8s.Provider,
    source_service_name: str,
    source_attribute_type: str,
    target_service_name: str,
    depends_on: list[pulumi.Resource] | None = None,
) -> k8s.yaml.v2.ConfigGroup:
    resource_name = (
        f"pocketsizefund-{source_service_name}-to-{target_service_name}-trigger"
    )

    content = {
        "apiVersion": "eventing.knative.dev/v1",
        "kind": "Trigger",
        "metadata": {
            "name": str(resource_name),
            "namespace": "default",
        },
        "spec": {
            "broker": "default",
            "filter": {
                "attributes": {
                    "type": source_attribute_type,  # dot separated
                },
            },
            "subscriber": {
                "ref": {
                    "apiVersion": "serving.knative.dev/v1",
                    "kind": "Service",
                    "name": target_service_name,
                }
            },
        },
    }

    return k8s.yaml.v2.ConfigGroup(
        resource_name=str(resource_name),
        objs=[content],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=depends_on,
        ),
    )


def create_knative_schedule(
    kubernetes_provider: k8s.Provider,
    target_service_name: str,
    target_path: str,
    cron_schedule: str,
    depends_on: list[pulumi.Resource] | None = None,
) -> k8s.yaml.v2.ConfigGroup:
    content = {
        "apiVersion": "sources.knative.dev/v1",
        "kind": "PingSource",
        "metadata": {
            "name": f"{target_service_name}-pingsource",
            "namespace": "default",
        },
        "spec": {
            "schedule": cron_schedule,
            "sink": {
                "ref": {
                    "apiVersion": "serving.knative.dev/v1",
                    "kind": "Service",
                    "name": target_service_name,
                },
                "uri": target_path,
            },
        },
    }

    formated_cron_schedule = cron_schedule.replace(" ", "-")

    return k8s.yaml.v2.ConfigGroup(
        resource_name=f"{target_service_name}-{formated_cron_schedule}-schedule",
        objs=[content],
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=depends_on,
        ),
    )
