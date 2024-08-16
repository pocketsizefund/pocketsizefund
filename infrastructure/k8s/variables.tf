variable "cluster_endpoint" {
  description = "The endpoint to use for the cluster"
  type        = string
}

variable "cluster_ca_certificate" {
  description = "The CA certificate to use for the cluster"
  type        = string
}
variable "cluster_name" {
  description = "The name of the cluster"
  type        = string
}

variable "cluster_oidc_issuer_url" {
  description = "The OIDC issuer URL to use for the cluster"
  type        = string
}

variable "cluster_oidc_provider_arn" {
  description = "The OIDC provider ARN to use for the cluster"
  type        = string
}

variable "cluster_security_group_id" {
  description = "The security group ID to use for the cluster"
  type        = string
}

variable "cluster_vpc_id" {
  description = "The VPC ID to use for the cluster"
  type        = string
}

variable "cluster_subnet_ids" {
  description = "The subnet IDs to use for the cluster"
  type        = list(string)
}

variable "grafana_admin_password" {
  description = "The Grafana admin password"
  type        = string
}

variable "grafana_cert_arn" {
  description = "The ARN of the Grafana certificate"
  type        = string
}

variable "route53_zone_id" {
  description = "The ID of the Route53 zone to use for the domain"
  type        = string
}

variable "domain_name" {
  description = "The fund domain name"
  type        = string
}
