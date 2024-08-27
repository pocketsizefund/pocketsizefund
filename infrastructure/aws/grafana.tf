resource "aws_security_group" "grafana_alb_sg" {
  name        = "grafana-alb-sg"
  description = "Security group for Grafana ALB"
  vpc_id      = aws_vpc.eks_vpc.id

  ingress {
    description = "HTTPS from anywhere"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "Grafana ALB Security Group"
  }
}

resource "aws_lb" "grafana" {
  name               = "grafana-alb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.grafana_alb_sg.id]
  subnets            = aws_subnet.eks_subnets[*].id

  enable_deletion_protection = true

  lifecycle {
    prevent_destroy       = false
    ignore_changes        = [name_prefix, internal, load_balancer_type, security_groups, subnets]
    create_before_destroy = true
  }

  tags = {
    Name = "Grafana ALB"
  }
}
