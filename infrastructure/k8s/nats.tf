resource "kubernetes_namespace" "event_bus" {
  metadata {
    name = "event-bus"
  }
}

resource "null_resource" "setup_event_bus" {
  triggers = {
    nats_namespace = kubernetes_namespace.event_bus.metadata[0].name
  }

  provisioner "local-exec" {
    command = <<-EOT
      helm repo add nats https://nats-io.github.io/k8s/helm/charts/
      helm repo update
      helm pull nats/nats --untar
      helm template nats ./nats \
        --namespace ${kubernetes_namespace.event_bus.metadata[0].name} \
        --set cluster.enabled=true \
        --set cluster.replicas=3 \
        --set nats.jetstream.enabled=true > nats-resources.yaml
      kubectl apply -f nats-resources.yaml
    EOT
  }
}

resource "null_resource" "create_event_bus_channel_crd" {
  depends_on = [null_resource.setup_event_bus]
  provisioner "local-exec" {
    command = <<-EOT
      cat <<EOF | kubectl apply -f -
      apiVersion: apiextensions.k8s.io/v1
      kind: CustomResourceDefinition
      metadata:
        name: natsschannels.messaging.knative.dev
      spec:
        group: messaging.knative.dev
        names:
          kind: NatssChannel
          plural: natsschannels
          singular: natsschannel
          listKind: NatssChannelList
        scope: Namespaced
        versions:
        - name: v1alpha1
          served: true
          storage: true
          schema:
            openAPIV3Schema:
              type: object
              properties:
                spec:
                  type: object
                  x-kubernetes-preserve-unknown-fields: true
      EOF
    EOT
  }
}


resource "null_resource" "configure_knative_event_bus" {
  provisioner "local-exec" {
    command = <<-EOT
      cat <<EOF | kubectl apply -f -
      ---
      apiVersion: v1
      kind: ConfigMap
      metadata:
        name: default-ch-webhook
        namespace: knative-eventing
      data:
        default-ch-config: |
          clusterDefault:
            apiVersion: messaging.knative.dev/v1alpha1
            kind: NatssChannel
      ---
      apiVersion: v1
      kind: ConfigMap
      metadata:
        name: config-br-defaults
        namespace: knative-eventing
      data:
        default-br-config: |
          clusterDefault:
            brokerClass: MTChannelBasedBroker
            apiVersion: v1
            kind: ConfigMap
            name: config-br-default-channel
            namespace: knative-eventing
      ---
      apiVersion: v1
      kind: ConfigMap
      metadata:
        name: config-br-default-channel
        namespace: knative-eventing
      data:
        channelTemplateSpec: |
          apiVersion: messaging.knative.dev/v1alpha1
          kind: NatssChannel
      EOF
    EOT
  }
}

resource "null_resource" "verify_event_bus_setup" {
  depends_on = [
    null_resource.setup_event_bus,
    null_resource.create_event_bus_channel_crd,
    null_resource.configure_knative_event_bus
  ]

  provisioner "local-exec" {
    command = <<-EOT
      kubectl get pods -n event-bus
      kubectl get crd natsschannels.messaging.knative.dev
      kubectl get configmap -n knative-eventing default-ch-webhook config-br-defaults config-br-default-channel
    EOT
  }
}

resource "null_resource" "setup_mt_channel_broker" {
  depends_on = [null_resource.configure_knative_event_bus]

  provisioner "local-exec" {
    command = <<-EOT
      kubectl apply -f https://github.com/knative/eventing/releases/download/knative-v1.8.0/mt-channel-broker.yaml
    EOT
  }
}

resource "null_resource" "verify_mt_channel_broker_setup" {
  depends_on = [
    null_resource.setup_event_bus,
    null_resource.create_event_bus_channel_crd,
    null_resource.configure_knative_event_bus,
    null_resource.setup_mt_channel_broker
  ]

  provisioner "local-exec" {
    command = <<-EOT
      kubectl get pods -n event-bus
      kubectl get crd natsschannels.messaging.knative.dev
      kubectl get configmap -n knative-eventing default-ch-webhook config-br-defaults config-br-default-channel
      kubectl get pods -n knative-eventing | grep mt-broker
    EOT
  }
}
