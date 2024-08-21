resource "kubernetes_namespace" "knative_serving" {
  metadata {
    name = "knative-serving"
  }
}

resource "kubernetes_namespace" "kourier_system" {
  metadata {
    name = "kourier-system"
  }
}


resource "null_resource" "apply_serving_crds" {
  depends_on = [kubernetes_namespace.knative_serving, kubernetes_namespace.kourier_system]

  provisioner "local-exec" {
    command = "kubectl apply -f https://github.com/knative/serving/releases/download/knative-v1.15.2/serving-crds.yaml"
  }
}

resource "null_resource" "apply_serving_core" {
  depends_on = [null_resource.apply_serving_crds]

  provisioner "local-exec" {
    command = "kubectl apply -f https://github.com/knative/serving/releases/download/knative-v1.15.2/serving-core.yaml"
  }
}

resource "null_resource" "apply_kourier" {
  depends_on = [null_resource.apply_serving_core]

  provisioner "local-exec" {
    command = "kubectl apply -f https://github.com/knative/net-kourier/releases/download/knative-v1.15.1/kourier.yaml"
  }
}

resource "null_resource" "patch_kourier" {
  depends_on = [null_resource.apply_kourier]

  provisioner "local-exec" {
    command = <<-EOT
      kubectl patch configmap/config-network \
          --namespace knative-serving \
          --type merge \
          --patch '{"data":{"ingress-class":"kourier.ingress.networking.knative.dev"}}'
    EOT
  }
}


# TODO: set up real DNS at some point
resource "null_resource" "magic_dns" {
  depends_on = [null_resource.patch_kourier]

  provisioner "local-exec" {
    command = <<-EOT
      kubectl apply -f https://github.com/knative/serving/releases/download/knative-v1.15.2/serving-default-domain.yaml
    EOT
  }
}

resource "null_resource" "apply_hpa" {
  depends_on = [null_resource.apply_kourier]
  provisioner "local-exec" {
    command = <<-EOT
      kubectl apply -f https://github.com/knative/serving/releases/download/knative-v1.15.2/serving-hpa.yaml
    EOT
  }
}


resource "null_resource" "verify_knative_serving_installation" {
  depends_on = [null_resource.patch_kourier]

  provisioner "local-exec" {
    command = <<EOT
      [ $(kubectl get pods -n knative-serving --no-headers | wc -l) -gt 0 ] || (echo "Error: No Knative Serving Pods Found" && exit 1)
      [ $(kubectl get pods -n kourier-system --no-headers | wc -l) -gt 0 ] || (echo "Error: No Kourier Pods Found" && exit 1)
      [ $(kubectl get service -n kourier-system kourier --no-headers | wc -l) -gt 0 ] || (echo "Error: No Kourier Service Found" && exit 1)
      echo "Knative Serving Installation Verified"
    EOT
  }
}

resource "kubernetes_namespace" "knative_eventing" {
  metadata {
    name = "knative-eventing"
  }
}

resource "null_resource" "apply_eventing_crds" {
  depends_on = [kubernetes_namespace.knative_eventing]

  provisioner "local-exec" {
    command = "kubectl apply -f https://github.com/knative/eventing/releases/download/knative-v1.15.0/eventing-crds.yaml"
  }
}

resource "null_resource" "install_eventing_core" {
  depends_on = [null_resource.apply_eventing_crds]

  provisioner "local-exec" {
    command = "kubectl apply -f https://github.com/knative/eventing/releases/download/knative-v1.15.0/eventing-core.yaml"
  }
}


resource "null_resource" "verify_eventing_installation" {
  depends_on = [null_resource.install_eventing_core]
  provisioner "local-exec" {
    command = <<EOT
      [ $(kubectl get pods -n knative-eventing --no-headers | wc -l) -gt 0 ] || (echo "Error: No Knative Eventing Pods Found" && exit 1)
      echo "Knative Eventing Installation Verified"
    EOT
  }
}


