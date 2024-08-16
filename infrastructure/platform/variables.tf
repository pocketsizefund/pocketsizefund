variable "cluster_endpoint" {
  type = string
}

variable "cluster_ca_certificate" {
  type = string
}
variable "cluster_name" {
  type = string
}

variable "environment" {
  description = "Environment"
  type        = string
  sensitive   = true
}

variable "sentry_dsn" {
  description = "Sentry DSN"
  type        = string
  sensitive   = true
}

variable "darqube_api_key" {
  description = "Darqube API Key"
  type        = string
  sensitive   = true
}

variable "alpaca_api_key" {
  description = "Alpaca API Key"
  type        = string
  sensitive   = true
}

variable "alpaca_api_secret" {
  description = "Alpaca API Secret"
  type        = string
  sensitive   = true
}

variable "alpha_vantage_api_key" {
  description = "Alpha Vantage API Key"
  type        = string
  sensitive   = true
}

variable "edgar_user_agent" {
  description = "EDGAR User Agent"
  type        = string
  sensitive   = true
}

variable "model_file_name" {
  description = "Model File Name"
  type        = string
}

