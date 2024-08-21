resource "kubernetes_namespace" "monitoring" {
  metadata {
    name = "monitoring"
  }
}

resource "aws_acm_certificate" "grafana" {
  domain_name       = "grafana.${var.domain_name}"
  validation_method = "EMAIL"

  tags = {
    Name = "Grafana Certificate"
  }

  lifecycle {
    create_before_destroy = true
  }
}
