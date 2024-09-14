package { ['eksctl', 'yq']:
  ensure => 'installed',
}

$cluster_config = yaml_data('/path/to/cluster-config.yaml')
$cluster_name = $cluster_config['metadata']['name']
$aws_region = $cluster_config['metadata']['region']

notify { "creating s3 buckets in ${cluster_name} in ${aws_region}": }

exec { 'manage_eks_cluster':
  command => $facts['eksctl_cluster_exists'] ? {
    true    => "eksctl upgrade cluster -f /path/to/cluster-config.yaml",
    default => "eksctl create cluster -f /path/to/cluster-config.yaml",
  },
  path    => ['/usr/bin', '/usr/local/bin'],
  require => Package['eksctl'],
}

facter::fact { 'eksctl_cluster_exists':
  value => "eksctl get cluster --name=${cluster_name} --region=${aws_region} > /dev/null 2>&1 && echo true || echo false",
}
