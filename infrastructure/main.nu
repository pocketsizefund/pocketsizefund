def create-contexts [] {
  do {
    docker context rm -f pocketsizefund
    docker context rm -f pocketsizefund-local
  } | ignore
  docker context create pocketsizefund --docker "host=ssh://pocketsizefund-swarm"
  docker context create pocketsizefund-local
}

def launch-stacks [context: string] {
  docker context use $context
  docker stack deploy -c stack.yml infrastructure --detach=false
}

def "infrastructure up" [] {
  ^pulumi up --yes
  let manager_ip = (^pulumi stack output managerIp | str trim)
  let pem = (^pulumi stack output --show-secrets sshPrivateKeyPem)

  $pem | save --raw --force swarm.pem
  chmod 600 swarm.pem

  let ssh_cfg = $"($env.HOME)/.ssh/config"
  let block = $"
Host pocketsizefund-swarm
  HostName ($manager_ip)
  User ubuntu
  IdentityFile ($env.PWD)/swarm.pem
  IdentitiesOnly yes
  StrictHostKeyChecking accept-new
"
  if (ls $ssh_cfg | is-empty) {
    $block | save --raw --force $ssh_cfg
}   else {
      let cfg = (open --raw $ssh_cfg)
      let filtered = ($cfg
      | lines
      | reduce -f [] { |it, acc|
          if ($it | str starts-with "Host pocketsizefund-swarm") { $acc } else { $acc | append $it }
        }
      | str join (char nl))
    $filtered | save --raw --force $ssh_cfg
    $"\n($block)" | save --raw --append $ssh_cfg
  }

  ssh-keygen -R $manager_ip | ignore
  ssh-keyscan -H $manager_ip | save --append $"($env.HOME)/.ssh/known_hosts"

  ssh pocketsizefund-swarm 'docker info -f "{{.ServerVersion}} {{.Swarm.LocalNodeState}}"'

  create-contexts

  launch-stacks pocketsizefund
  launch-stacks pocketsizefund-local

  docker node ls
}
