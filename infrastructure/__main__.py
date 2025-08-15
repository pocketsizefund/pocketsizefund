import pulumi
import pulumi_aws as aws
import pulumi_tls as tls
from pulumi_command import remote

az = pulumi.Config().get("az") or "us-east-1a"
blueprint_id = pulumi.Config().get("blueprintId") or "ubuntu_24_04"
bundle_mgr = pulumi.Config().get("bundleIdMgr") or "medium_2_0"
bundle_wkr = pulumi.Config().get("bundleIdWkr") or "small_2_0"

ssh_key = tls.PrivateKey("swarm-key", algorithm="RSA", rsa_bits=4096)
ls_key = aws.lightsail.KeyPair(
    "swarm-ls-key",
    name="swarm-ls-key",
    public_key=ssh_key.public_key_openssh,
)

cloud_init = """#cloud-config
package_update: true
package_upgrade: true
write_files:
  - path: /usr/local/bin/install-docker.sh
    permissions: "0755"
    content: |
      #!/usr/bin/env bash
      set -euo pipefail
      retry() { for i in {1..10}; do "$@" && break || { sleep 3; echo "retry $i"; }; done; }

      retry apt-get update
      retry apt-get install -y ca-certificates curl gnupg

      install -m 0755 -d /etc/apt/keyrings
      curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg
      chmod a+r /etc/apt/keyrings/docker.gpg

      . /etc/os-release
      echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu ${VERSION_CODENAME} stable" > /etc/apt/sources.list.d/docker.list

      retry apt-get update
      retry apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

      usermod -aG docker ubuntu || true
      systemctl enable --now docker

runcmd:
  - /usr/local/bin/install-docker.sh
  - bash -lc 'for i in {1..60}; do [ -x /usr/bin/docker ] && systemctl is-active --quiet docker && exit 0 || sleep 2; done; exit 1'
"""


def mk_instance(name: str, bundle_id: str):
    inst = aws.lightsail.Instance(
        name,
        name=name,
        availability_zone=az,
        blueprint_id=blueprint_id,
        bundle_id=bundle_id,
        key_pair_name=ls_key.name,
        user_data=cloud_init,
        tags={"role": name},
    )

    aws.lightsail.InstancePublicPorts(
        f"{name}-ports",
        instance_name=inst.name,
        port_infos=[
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=22, to_port=22, protocol="tcp", cidrs=["0.0.0.0/0"]
            ),
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=2377, to_port=2377, protocol="tcp", cidrs=["0.0.0.0/0"]
            ),
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=7946, to_port=7946, protocol="tcp", cidrs=["0.0.0.0/0"]
            ),
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=7946, to_port=7946, protocol="udp", cidrs=["0.0.0.0/0"]
            ),
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=4789, to_port=4789, protocol="udp", cidrs=["0.0.0.0/0"]
            ),
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=80, to_port=80, protocol="tcp", cidrs=["0.0.0.0/0"]
            ),
            aws.lightsail.InstancePublicPortsPortInfoArgs(
                from_port=443, to_port=443, protocol="tcp", cidrs=["0.0.0.0/0"]
            ),
        ],
    )

    ip = aws.lightsail.StaticIp(f"{name}-ip", name=f"{name}-ip")
    aws.lightsail.StaticIpAttachment(
        f"{name}-ip-attach",
        instance_name=inst.name,
        static_ip_name=ip.name,
    )

    return inst, ip


mgr_inst, mgr_ip = mk_instance("swarm-mgr-1", bundle_mgr)
w1_inst, w1_ip = mk_instance("swarm-wkr-1", bundle_wkr)
w2_inst, w2_ip = mk_instance("swarm-wkr-2", bundle_wkr)


def conn(host_output: pulumi.Output[str]) -> remote.ConnectionArgs:
    return remote.ConnectionArgs(
        host=host_output,
        user="ubuntu",
        private_key=ssh_key.private_key_pem,
    )


init_manager = remote.Command(
    "init-manager",
    connection=conn(mgr_ip.ip_address),
    create=" && ".join(
        [
            "bash -lc 'for i in {1..120}; do sudo docker info >/dev/null 2>&1 && break || sleep 3; done'",
            "bash -lc 'PUBIP=$(curl -s http://169.254.169.254/latest/meta-data/public-ipv4); echo Using-Public-IP:$PUBIP'",
            'bash -lc \'STATE=$(sudo docker info --format "{{.Swarm.LocalNodeState}}") || true; '
            'CTRL=$(sudo docker info --format "{{.Swarm.ControlAvailable}}") || true; '
            '[ "$STATE" = active -a "$CTRL" = true ] || '
            "(sudo docker swarm leave --force || true; "
            ' sudo docker swarm init --advertise-addr "$PUBIP" --listen-addr "0.0.0.0:2377")\'',
            "bash -lc 'for i in {1..30}; do sudo ss -ltn | awk \"\\$4 ~ /:2377$/\" && break || sleep 2; done'",
            "bash -lc 'sudo docker swarm join-token -q worker | sudo tee /home/ubuntu/worker.token >/dev/null'",
            "bash -lc 'sudo docker swarm join-token -q manager | sudo tee /home/ubuntu/manager.token >/dev/null'",
        ]
    ),
)


get_worker_token = remote.Command(
    "get-worker-token",
    connection=conn(mgr_ip.ip_address),
    create="bash -lc 'sudo docker swarm join-token -q worker'",
    opts=pulumi.ResourceOptions(depends_on=[init_manager]),
)
worker_token = pulumi.Output.secret(get_worker_token.stdout).apply(lambda s: s.strip())


def join_worker(res_name: str, worker_ip: pulumi.Output[str]) -> remote.Command:
    create_cmd = pulumi.Output.all(mgr_ip.ip_address, worker_token).apply(
        lambda vals: (
            "bash -lc 'for i in {1..120}; do sudo docker info >/dev/null 2>&1 && break || sleep 3; done && "
            f"sudo docker swarm join --token {vals[1]} {vals[0]}:2377'"
        )
    )
    return remote.Command(
        res_name,
        connection=conn(worker_ip),
        create=create_cmd,
        opts=pulumi.ResourceOptions(depends_on=[get_worker_token]),
    )


join_w1 = join_worker("join-w1", w1_ip.ip_address)
join_w2 = join_worker("join-w2", w2_ip.ip_address)

pulumi.export("managerIp", mgr_ip.ip_address)
pulumi.export("workerIps", pulumi.Output.all(w1_ip.ip_address, w2_ip.ip_address))
pulumi.export("sshPrivateKeyPem", pulumi.Output.secret(ssh_key.private_key_pem))
