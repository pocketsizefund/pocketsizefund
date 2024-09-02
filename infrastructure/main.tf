provider "aws" {
  region = var.aws_region
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

terraform {
  cloud {

    organization = "pocketsizefund"

    workspaces {
      name = "platform"
    }
  }
}


data "aws_route53_zone" "main" {
  name = var.domain_name
}

module "aws" {
  source         = "./aws"
  domain_name    = var.domain_name
  domain_contact = var.domain_contact
  vpc_cidr       = "10.0.0.0/16"
}


module "k8s" {
  source = "./k8s"

  cluster_endpoint          = module.aws.cluster_endpoint
  cluster_ca_certificate    = module.aws.cluster_ca_certificate
  cluster_name              = module.aws.cluster_name
  cluster_oidc_issuer_url   = module.aws.cluster_oidc_issuer_url
  cluster_oidc_provider_arn = module.aws.cluster_oidc_provider_arn
  cluster_security_group_id = module.aws.cluster_security_group_id
  cluster_vpc_id            = module.aws.vpc_id
  cluster_subnet_ids        = module.aws.private_subnet_ids
  grafana_admin_password    = var.grafana_admin_password
  grafana_cert_arn          = module.aws.grafana_cert_arn
  route53_zone_id           = data.aws_route53_zone.main.id
  domain_name               = var.domain_name
}

module "platform" {
  source                 = "./platform"
  cluster_endpoint       = module.aws.cluster_endpoint
  cluster_ca_certificate = module.aws.cluster_ca_certificate
  cluster_name           = module.aws.cluster_name
  environment            = var.environment
  sentry_dsn             = var.sentry_dsn
  darqube_api_key        = var.darqube_api_key
  alpaca_api_key         = var.alpaca_api_key
  alpaca_api_secret      = var.alpaca_api_secret
  alpha_vantage_api_key  = var.alpha_vantage_api_key
  edgar_user_agent       = var.edgar_user_agent
  model_file_name        = var.model_file_name
  s3_data_bucket_name    = var.s3_data_bucket_name
}
