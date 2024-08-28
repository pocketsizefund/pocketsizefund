resource "kubernetes_namespace" "live" {
  metadata {
    name = "live"
  }
}

provider "kubernetes" {
  host                   = var.cluster_endpoint
  cluster_ca_certificate = base64decode(var.cluster_ca_certificate)
  exec {
    api_version = "client.authentication.k8s.io/v1beta1"
    command     = "aws"
    args        = ["eks", "get-token", "--cluster-name", var.cluster_name]
  }
}

resource "null_resource" "price_model_service" {
  depends_on = [kubernetes_namespace.live]

  provisioner "local-exec" {
    command = "kubectl apply -f platform/price-model.yaml --namespace live"
  }
}

resource "null_resource" "position_manager_service" {
  depends_on = [kubernetes_namespace.live]

  provisioner "local-exec" {
    command = "kubectl apply -f platform/position-manager.yaml --namespace live"
  }
}

resource "null_resource" "chronos_service" {
  depends_on = [kubernetes_namespace.live]

  provisioner "local-exec" {
    command = "kubectl apply -f platform/chronos.yaml --namespace live"
  }
}

resource "null_resource" "discord_service" {
  depends_on = [kubernetes_namespace.live]

  provisioner "local-exec" {
    command = "kubectl apply -f platform/discord.yaml --namespace live"
  }
}
