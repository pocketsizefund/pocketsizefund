import asyncio
import base64
import os
import shutil
import subprocess
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Literal

import boto3
import httpx
import typer
from botocore.exceptions import ClientError
from fabric import Connection
from pydantic import BaseModel
from structlog import get_logger
from tailscale import Tailscale

logger = get_logger()

SWARM_MANAGER_ALLOWED_CIDRS = ["10.0.0.0/8", "192.168.0.0/16", "172.16.0.0/12"]
SWARM_CLUSTER_ALLOWED_CIDRS = ["10.0.0.0/16"]
OPEN_TO_WORLD = ["0.0.0.0/0"]

LIGHTSAIL_KEY_NAME = os.environ.get("LIGHTSAIL_KEY_NAME", "swarm-ls-key")
PRIVATE_KEY_PATH = Path(
    os.environ.get("PRIVATE_KEY_PATH") or Path("~/.ssh/swarm-key.pem").expanduser()
)


class Instance(BaseModel):
    name: str
    role: Literal["manager", "worker"] = "worker"
    region: str = "us-east-1"
    availability_zone: str = "us-east-1a"
    blueprint: str = "ubuntu_24_04"
    bundle: Literal["small_2_0", "medium_2_0"] = "small_2_0"
    public_ip: str | None = None
    tailscale_ip: str | None = None


def remove_instance(instance: Instance) -> None:
    lightsail = boto3.client("lightsail", region_name=instance.region)

    try:
        lightsail.delete_instance(instanceName=instance.name)
    except ClientError as e:
        if e.response["Error"]["Code"] != "NotFoundException":
            logger.exception("error deleting %s", instance.name)


async def remove_tailscale_node(instance: Instance) -> None:
    async with Tailscale(
        tailnet=os.getenv("TAILSCALE_TAILNET"),
        api_key=os.getenv("TAILSCALE_API_KEY"),
    ) as tailscale:
        devices = await tailscale.devices()
        for device in devices.values():
            if device.name.split(".")[0] == instance.name:
                logger.info(
                    "Removing Tailscale device %s (ID: %s)",
                    device.name,
                    device.device_id,
                )

                headers = {"Authorization": f"Bearer {tailscale.api_key}"}
                async with httpx.AsyncClient() as client:
                    url = f"https://api.tailscale.com/api/v2/device/{device.device_id}"
                    response = await client.delete(url, headers=headers)
                    http_ok = 200
                    if response.status_code == http_ok:
                        logger.info(
                            "Successfully removed Tailscale device %s", device.name
                        )
                    else:
                        logger.error(
                            "Failed to remove device %s: HTTP %d",
                            device.name,
                            response.status_code,
                        )
                return


def release_static_ip(instance: Instance) -> None:
    lightsail = boto3.client("lightsail", region_name=instance.region)
    ip_name = f"{instance.name}-ip"
    try:
        lightsail.release_static_ip(staticIpName=ip_name)
    except ClientError as e:
        if e.response["Error"]["Code"] != "NotFoundException":
            logger.exception("Error releasing %s", ip_name)


def ensure_ls_keypair() -> None:  # noqa: C901, PLR0912, PLR0915
    """
    Ensure a Lightsail keypair exists with name LIGHTSAIL_KEY_NAME.
    If missing, create it and write the private key locally.
    """
    if PRIVATE_KEY_PATH.exists():
        logger.info("using existing local private key=%s", PRIVATE_KEY_PATH)
        return

    lightsail = boto3.client("lightsail", region_name="us-east-1")
    try:
        lightsail.get_key_pair(keyPairName=LIGHTSAIL_KEY_NAME)
        logger.info(
            "keypair %s exists in Lightsail but no local key found.", LIGHTSAIL_KEY_NAME
        )
        logger.info(
            "deleting existing keypair to recreate with downloadable private key..."
        )
        lightsail.delete_key_pair(keyPairName=LIGHTSAIL_KEY_NAME)
        logger.info("deleted existing keypair %s", LIGHTSAIL_KEY_NAME)
    except ClientError as e:
        if e.response["Error"]["Code"] == "NotFoundException":
            logger.warning(
                "keypair %s does not exist, will create it.", LIGHTSAIL_KEY_NAME
            )
        else:
            raise

    try:
        logger.info("creating Lightsail keypair: %s", LIGHTSAIL_KEY_NAME)
        resp = lightsail.create_key_pair(keyPairName=LIGHTSAIL_KEY_NAME)

        # AWS Lightsail returns the private key in privateKeyBase64 field
        # Despite the name, it might already be in PEM format (not base64 encoded)
        if "privateKeyBase64" in resp:
            pk_content = resp["privateKeyBase64"]

            # Check if it's already in PEM format
            if pk_content.startswith("-----BEGIN"):
                pem_content = pk_content
            else:
                try:
                    pk_b64 = (
                        pk_content.replace("\n", "").replace("\r", "").replace(" ", "")
                    )

                    missing_padding = len(pk_b64) % 4
                    if missing_padding:
                        pk_b64 += "=" * (4 - missing_padding)

                    pem_bytes = base64.b64decode(pk_b64)
                    pem_content = pem_bytes.decode("utf-8")
                except Exception:
                    logger.exception("Error decoding base64")
                    logger.warning("content length: %d", len(pk_content))
                    logger.warning("first 100 chars: %s", pk_content[:100])
                    logger.warning("treating as PEM content directly")
                    # Fall back to using it directly
                    pem_content = pk_content
        elif "privateKey" in resp:
            pem_content = resp["privateKey"]
        else:
            logger.info("response keys: %s", resp.keys())

            # Large content threshold for finding potential keys
            large_content_threshold = 1000

            for key, value in resp.items():
                is_str = isinstance(value, str)
                is_large = is_str and len(value) > large_content_threshold
                if is_large and "BEGIN" in value:
                    logger.info(
                        "found potential key in field '%s': %s...", key, value[:100]
                    )

            msg = "No private key found in response"
            raise ValueError(msg)

        PRIVATE_KEY_PATH.parent.mkdir(mode=0o700, parents=True, exist_ok=True)

        PRIVATE_KEY_PATH.write_text(pem_content)
        PRIVATE_KEY_PATH.chmod(0o600)
        logger.info("wrote private key to: %s", PRIVATE_KEY_PATH)
    except ClientError as e:
        logger.info("failed to create keypair: %s", e)
        raise


def create_instance(instance: Instance) -> Instance:
    lightsail = boto3.client("lightsail", region_name=instance.region)

    try:
        existing = lightsail.get_instance(instanceName=instance.name)
        state = existing["instance"]["state"]["name"]
        logger.info("Instance %s already exists in state: %s", instance.name, state)

        while state != "running":
            logger.info(
                "Waiting for existing instance %s to reach running state...",
                instance.name,
            )
            time.sleep(2)
            result = lightsail.get_instance(instanceName=instance.name)
            state = result["instance"]["state"]["name"]

    except ClientError as e:
        if e.response["Error"]["Code"] != "NotFoundException":
            raise
    else:
        return instance

    logger.info("Creating new instance: %s", instance.name)
    try:
        lightsail.create_instances(
            instanceNames=[instance.name],
            availabilityZone=instance.availability_zone,
            blueprintId=instance.blueprint,
            bundleId=instance.bundle,
            keyPairName=LIGHTSAIL_KEY_NAME,
            tags=[{"key": "role", "value": instance.role}],
        )
    except ClientError as e:
        if "already in use" in str(e):
            logger.info(
                "Instance %s was created by another process, continuing...",
                instance.name,
            )
            result = lightsail.get_instance(instanceName=instance.name)
            state = result["instance"]["state"]["name"]
        else:
            raise

    state = "pending"
    while state != "running":
        state = lightsail.get_instance(instanceName=instance.name)["instance"]["state"]["name"]
        time.sleep(0.25)

    return instance


def create_network(instance: Instance) -> Instance:
    lightsail = boto3.client("lightsail", region_name=instance.region)
    ip_name = f"{instance.name}-ip"

    lightsail.put_instance_public_ports(
        portInfos=[
            {"fromPort": 22, "toPort": 22, "protocol": "tcp", "cidrs": OPEN_TO_WORLD},
            {"fromPort": 80, "toPort": 80, "protocol": "tcp", "cidrs": OPEN_TO_WORLD},
            {"fromPort": 443, "toPort": 443, "protocol": "tcp", "cidrs": OPEN_TO_WORLD},
        ],
        instanceName=instance.name,
    )

    try:
        lightsail.allocate_static_ip(staticIpName=ip_name)
        logger.info("Allocated new static IP: %s", ip_name)
    except ClientError as e:
        error_code = e.response["Error"]["Code"]
        if error_code == "AlreadyExistsException" or "already in use" in str(e):
            logger.warning("Static IP %s already exists, continuing...", ip_name)
        else:
            raise

    try:
        lightsail.attach_static_ip(staticIpName=ip_name, instanceName=instance.name)
        logger.info("Attached static IP %s to %s", ip_name, instance.name)
    except ClientError as e:
        error_code = e.response["Error"]["Code"]
        already_attached = "already attached" in str(e).lower()
        if already_attached or error_code == "InvalidInputException":
            logger.warning(
                "Static IP %s already attached to %s, continuing...",
                ip_name,
                instance.name,
            )
        else:
            raise

    instance.public_ip = lightsail.get_static_ip(staticIpName=ip_name)["staticIp"]["ipAddress"]
    return instance


def run_setup_script(instance: Instance) -> Instance:
    tailscale_auth_key = os.getenv("TAILSCALE_AUTH_KEY")

    max_retries = 20
    retry_delay = 15

    for attempt in range(max_retries):
        try:
            with Connection(
                host=instance.public_ip,
                user="ubuntu",
                connect_kwargs={
                    "key_filename": str(PRIVATE_KEY_PATH),
                    "banner_timeout": 30,
                    "auth_timeout": 30,
                },
                connect_timeout=10,
            ) as ssh:
                logger.info("Connected to %s via SSH", instance.name)
                break
        except Exception:
            if attempt < max_retries - 1:
                logger.info(
                    "SSH connection to %s failed (attempt %d/%d), retrying in %ds...",
                    instance.name,
                    attempt + 1,
                    max_retries,
                    retry_delay,
                )
                time.sleep(retry_delay)
            else:
                logger.exception(
                    "Failed to connect to %s after %d attempts",
                    instance.name,
                    max_retries,
                )
                raise

    with Connection(
        host=instance.public_ip,
        user="ubuntu",
        connect_kwargs={
            "key_filename": str(PRIVATE_KEY_PATH),
            "banner_timeout": 30,
            "auth_timeout": 30,
        },
        connect_timeout=10,
    ) as ssh:
        ssh.run("sudo DEBIAN_FRONTEND=noninteractive apt-get update && sudo DEBIAN_FRONTEND=noninteractive apt-get -y upgrade")

        ssh.run(
            "curl -fsSL https://get.docker.com -o get-docker.sh && sudo sh get-docker.sh"
        )
        ssh.run("sudo usermod -aG docker $USER")
        ssh.run("sudo systemctl enable docker && sudo systemctl restart docker")

        ssh.run("sudo mkdir -p /etc/docker")
        ssh.run(
            "echo '{"
            '"swarm-default-advertise-addr": "tailscale0", '
            '"mtu": 1280'
            "}' | sudo tee /etc/docker/daemon.json"
        )
        ssh.run("sudo systemctl restart docker")

        ssh.run("curl -fsSL https://tailscale.com/install.sh | sh")
        ssh.run(
            f"sudo tailscale up --auth-key={tailscale_auth_key} "
            f"--hostname={instance.name} --ssh --accept-routes"
        )

        instance.tailscale_ip = ssh.run("tailscale ip -4 | head -n1").stdout.strip()

    return instance


def create_swarm(instance: Instance) -> tuple[str, str]:
    with Connection(
        host=instance.public_ip,
        user="ubuntu",
        connect_kwargs={
            "key_filename": str(PRIVATE_KEY_PATH),
            "banner_timeout": 30,
            "auth_timeout": 30,
        },
        connect_timeout=10,
    ) as ssh:
        try:
            result = ssh.run(
                f"docker swarm init --advertise-addr {instance.tailscale_ip}", warn=True
            )
            if result.failed:
                logger.warning("Node already part of a swarm, continuing...")
        except Exception:
            logger.exception("Swarm init failed (likely already initialized)")

        manager_token = ssh.run("docker swarm join-token -q manager").stdout.strip()
        worker_token = ssh.run("docker swarm join-token -q worker").stdout.strip()

    return manager_token, worker_token


def join_worker(worker: Instance, manager: Instance, token: str) -> None:
    host = worker.tailscale_ip
    with Connection(
        host=host,
        user="ubuntu",
        connect_kwargs={"key_filename": str(PRIVATE_KEY_PATH)},
    ) as ssh:
        ssh.run(
            "bash -lc 'for i in {1..120}; do sudo docker info >/dev/null 2>&1 "
            "&& break || sleep 3; done'"
        )

        result = ssh.run("docker info --format '{{.Swarm.LocalNodeState}}'", hide=True)
        state = result.stdout.strip()
        if state.lower() != "inactive":
            ssh.run("sudo docker swarm leave --force")

        ssh.run(f"sudo docker swarm join --token {token} {manager.tailscale_ip}:2377")


def create_docker_context(manager: Instance) -> None:
    """Create a Docker context using Tailscale DNS name"""
    tailscale_tailnet_id = os.getenv("TAILSCALE_TAILNET_ID")
    docker = shutil.which("docker")

    if not docker:
        logger.error("Docker executable not found")
        return

    subprocess.run(  # noqa: S603
        [docker, "context", "rm", "pocketsizefund", "--force"],
        check=False,
        capture_output=True,
    )

    tailscale_hostname = f"{manager.name}.{tailscale_tailnet_id}.ts.net"
    docker_host = f"ssh://ubuntu@{tailscale_hostname}"

    result = subprocess.run(  # noqa: S603
        [
            docker,
            "context",
            "create",
            "pocketsizefund",
            "--docker",
            f"host={docker_host}",
        ],
        check=False,
        capture_output=True,
        text=True,
    )

    if result.returncode == 0:
        logger.info(
            "Successfully created Docker context 'pocketsizefund' -> %s", docker_host
        )

        subprocess.run(  # noqa: S603
            [docker, "context", "use", "pocketsizefund"],
            capture_output=True,
            check=False,
        )
        logger.info("Switched to pocketsizefund Docker context")
    else:
        logger.info("Failed to create Docker context: %s", result.stderr)
        logger.info("Command output: %s", result.stdout)


app = typer.Typer()


def run_cleanup_tasks(instances: list[Instance]) -> None:
    """Run cleanup tasks in parallel"""
    with ThreadPoolExecutor(max_workers=3) as executor:
        instance_futures = [
            executor.submit(remove_instance, instance) for instance in instances
        ]
        ip_futures = [
            executor.submit(release_static_ip, instance) for instance in instances
        ]
        tailscale_futures = [
            executor.submit(asyncio.run, remove_tailscale_node(instance))
            for instance in instances
        ]

        for future in instance_futures + ip_futures + tailscale_futures:
            try:
                future.result()
            except Exception:
                logger.exception("Error during cleanup")


def setup_instance_chain(instance: Instance) -> Instance:
    """Set up an instance: create -> network -> setup script"""
    instance = create_instance(instance)
    instance = create_network(instance)
    return run_setup_script(instance)


def _remove_ssh_host_keys(manager_alias: str, tailscale_ip: str | None) -> None:
    """Remove existing SSH host keys."""
    ssh_keygen = shutil.which("ssh-keygen")
    if not ssh_keygen:
        return

    subprocess.run(  # noqa: S603
        [ssh_keygen, "-R", manager_alias],
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    if tailscale_ip:
        subprocess.run(  # noqa: S603
            [ssh_keygen, "-R", tailscale_ip],
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )


def _add_ssh_host_key(manager_alias: str) -> None:
    """Add new host key to known_hosts."""
    ssh_keyscan = shutil.which("ssh-keyscan")
    if not ssh_keyscan:
        return

    known_hosts = Path.home() / ".ssh" / "known_hosts"
    try:
        with subprocess.Popen(  # noqa: S603
            [ssh_keyscan, "-H", manager_alias],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        ) as proc:
            if proc.stdout:
                output = proc.stdout.read()
                if output:
                    with known_hosts.open("ab") as f:
                        f.write(output)
    except Exception:
        logger.exception("Failed to add host key to known_hosts")


def _update_ssh_config(manager_alias: str, tailscale_tailnet_id: str | None) -> None:
    """Update SSH config with manager host entry."""
    ssh_config_path = Path.home() / ".ssh" / "config"
    ssh_config_path.parent.mkdir(mode=0o700, exist_ok=True)

    config_lines = []
    if ssh_config_path.exists():
        with ssh_config_path.open() as f:
            config_lines = f.readlines()

    new_config_lines = []
    in_block = False
    for line in config_lines:
        if line.strip().startswith(f"Host {manager_alias}"):
            in_block = True
            continue
        if in_block:
            line_starts_host = line.strip().startswith("Host ")
            line_not_target = not line.strip().startswith(f"Host {manager_alias}")
            if line_starts_host and line_not_target:
                in_block = False
                new_config_lines.append(line)
            elif line.strip() == "":
                in_block = False
        else:
            new_config_lines.append(line)

    new_config_lines.append(f"Host {manager_alias}\n")
    new_config_lines.append(f"    HostName swarm-manager-0.{tailscale_tailnet_id}\n")
    new_config_lines.append("    User ubuntu\n")
    new_config_lines.append(f"    IdentityFile {PRIVATE_KEY_PATH}\n")
    new_config_lines.append("    IdentitiesOnly yes\n")
    new_config_lines.append("    StrictHostKeyChecking accept-new\n")
    new_config_lines.append("\n")

    with ssh_config_path.open("w") as f:
        f.writelines(new_config_lines)


def connect_local(manager: Instance) -> None:
    manager_alias = "swarm-manager"
    tailscale_tailnet_id = os.getenv("TAILSCALE_TAILNET_ID")

    _remove_ssh_host_keys(manager_alias, manager.tailscale_ip)
    _add_ssh_host_key(manager_alias)
    _update_ssh_config(manager_alias, tailscale_tailnet_id)


@app.command()
def down() -> None:
    """Tear down the infrastructure"""
    manager = Instance(name="swarm-manager-0", bundle="medium_2_0")
    worker_0 = Instance(name="swarm-worker-0")
    worker_1 = Instance(name="swarm-worker-1")

    instances = [manager, worker_0, worker_1]

    logger.info("Tearing down infrastructure...")
    run_cleanup_tasks(instances)
    logger.info("Infrastructure torn down")


@app.command()
def up() -> None:
    """Set up the infrastructure"""
    manager = Instance(name="swarm-manager-0", bundle="medium_2_0")
    worker_0 = Instance(name="swarm-worker-0")
    worker_1 = Instance(name="swarm-worker-1")

    instances = [manager, worker_0, worker_1]

    logger.info("Setting up infrastructure...")

    with ThreadPoolExecutor(max_workers=3) as executor:
        futures = [
            executor.submit(setup_instance_chain, instance) for instance in instances
        ]
        results = []
        for instance, future in zip(instances, futures, strict=True):
            try:
                result = future.result()
                results.append(result)
            except Exception:
                logger.exception("Error setting up instance '%s'", instance.name)
                results.append(None)

    manager, worker_0, worker_1 = results

    if None in results:
        failed_instances = [
            instances[i].name for i, result in enumerate(results) if result is None
        ]
        logger.error("Failed to set up instances: %s", failed_instances)
        logger.error("Aborting infrastructure setup due to instance failures.")
        return

    logger.info("Creating Docker swarm...")
    _, worker_token = create_swarm(manager)

    logger.info("Joining workers to swarm...")
    with ThreadPoolExecutor(max_workers=2) as executor:
        executor.submit(join_worker, worker_0, manager, worker_token)
        executor.submit(join_worker, worker_1, manager, worker_token)

    logger.info("Creating Docker context...")
    create_docker_context(manager)

    connect_local(manager)

    logger.info("Infrastructure ready!")
    logger.info("Manager: %s", manager.tailscale_ip)
    logger.info("Workers: %s, %s", worker_0.tailscale_ip, worker_1.tailscale_ip)


if __name__ == "__main__":
    app()
