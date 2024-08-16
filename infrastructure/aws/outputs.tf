output "grafana_cert_arn" {
  description = "The ARN of the Grafana certificate, used to secure the Grafana ingress"
  value       = aws_acm_certificate.grafana.arn
}

output "cluster_endpoint" {
  description = "Endpoint for EKS control plane, used to connect to the cluster"
  value       = aws_eks_cluster.eks_cluster.endpoint
}

output "cluster_security_group_id" {
  description = "Security group ID attached to the EKS cluster, used to restrict access to the cluster"
  value       = aws_eks_cluster.eks_cluster.vpc_config[0].cluster_security_group_id
}

output "cluster_name" {
  description = "Kubernetes Cluster Name, used to identify the cluster"
  value       = aws_eks_cluster.eks_cluster.name
}

output "vpc_id" {
  description = "The ID of the VPC, used to create the cluster"
  value       = aws_vpc.eks_vpc.id
}


output "private_subnet_ids" {
  description = "The IDs of the private subnets, used to create the cluster"
  value       = aws_subnet.eks_subnets[*].id
}


output "cluster_ca_certificate" {
  description = "The base64 encoded certificate data required to communicate with the cluster"
  value       = aws_eks_cluster.eks_cluster.certificate_authority[0].data
  sensitive   = true
}

output "cluster_oidc_issuer_url" {
  description = "The URL on the EKS cluster OIDC issuer is used to authenticate to the cluster"
  value       = aws_eks_cluster.eks_cluster.identity[0].oidc[0].issuer
}

output "cluster_oidc_provider_arn" {
  description = "The ARN of the EKS cluster OIDC provider is use to authenticate to the cluster"
  value       = aws_eks_cluster.eks_cluster.identity[0].oidc[0].issuer
}
