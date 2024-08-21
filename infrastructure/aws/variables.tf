variable "domain_auto_renew" {
  description = "Whether to enable auto-renewal for the domain"
  type        = bool
  default     = true
}

variable "domain_privacy_protection" {
  description = "Whether to enable privacy protection for the domain"
  type        = bool
  default     = true
}

variable availability_zones {
  description = "The availability zones to use for the domain"
  type        = list(string)
  default     = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

variable "cluster_name" {
  description = "The name of the EKS cluster"
  type        = string
  default = "pocketsizefund"
}

variable "node_group_min_size" {
  description = "The minimum size of the node group"
  type        = number
  default = 3
}

variable "node_group_max_size" {
  description = "The maximum size of the node group"
  type        = number
  default = 6
}

variable "node_group_desired_size" {
  description = "The desired size of the node group"
  type        = number
  default = 3
}

variable "node_group_instance_type" {
  description = "The instance type to use for the node group"
  type        = string
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


variable "vpc_cidr" {
  description = "CIDR block for the VPC"
  type        = string
}

