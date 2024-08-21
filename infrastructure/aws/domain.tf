data "aws_route53_zone" "main" {
  name = var.domain_name
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
