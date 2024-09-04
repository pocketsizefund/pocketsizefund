resource "kubernetes_secret" "platform_secrets" {
  metadata {
    name      = "platform"
    namespace = kubernetes_namespace.live.metadata[0].name
  }

  data = {
    environment           = var.environment
    sentry_dsn            = var.sentry_dsn
    darqube_api_key       = var.darqube_api_key
    alpaca_api_key        = var.alpaca_api_key
    alpaca_api_secret     = var.alpaca_api_secret
    alpha_vantage_api_key = var.alpha_vantage_api_key
    edgar_user_agent      = var.edgar_user_agent
    model_file_name       = var.model_file_name
    s3_data_bucket_name   = var.s3_data_bucket_name
  }

  type = "Opaque"
}


