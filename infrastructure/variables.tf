variable "aws_region" {
  type    = string
  default = "us-east-1"
}

variable "availability_zones" {
  type    = list(string)
  default = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

variable "cluster_name" {
  type    = string
  default = "pocketsizefund"
}

variable "node_group_min_size" {
  type    = number
  default = 3
}

variable "node_group_max_size" {
  type    = number
  default = 6
}

variable "node_group_desired_size" {
  type    = number
  default = 3
}

variable "node_group_instance_type" {
  type    = string
  default = "t3.medium"
}


variable "domain_name" {
  description = "The domain name to register"
  type        = string
}

variable "domain_contact" {
  description = "Domain contact information"
  type = object({
    first_name     = string
    last_name      = string
    email          = string
    address_line_1 = string
    city           = string
    state          = string
    zip_code       = string
    country_code   = string
    phone_number   = string
  })
}


variable "grafana_admin_password" {
  type      = string
  sensitive = true
}

variable "sentry_dsn" {
  type      = string
  sensitive = true
}

variable "darqube_api_key" {
  type      = string
  sensitive = true
}

variable "alpaca_api_key" {
  type      = string
  sensitive = true
}


variable "alpaca_api_secret" {
  type      = string
  sensitive = true
}

variable "alpha_vantage_api_key" {
  type      = string
  sensitive = true
}

variable "edgar_user_agent" {
  type      = string
  sensitive = true
}

variable "model_file_name" {
  type      = string
  sensitive = true
}

variable "environment" {
  type      = string
  sensitive = true
}
