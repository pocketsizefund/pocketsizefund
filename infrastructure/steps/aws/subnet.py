from invoke import task
from steps import CLUSTER_NAME, ec2_client

@task
def create(c, vpc_id, cidr, az, index):
    try:
        subnet = ec2_client.create_subnet(
            VpcId=vpc_id,
            CidrBlock=cidr,
            AvailabilityZone=az
        )
        subnet_id = subnet['Subnet']['SubnetId']
        ec2_client.create_tags(Resources=[subnet_id], Tags=[
            {'Key': 'Name', 'Value': f"{CLUSTER_NAME}-subnet-{index + 1}"},
            {'Key': f"kubernetes.io/cluster/{CLUSTER_NAME}", 'Value': "shared"}
        ])
        ec2_client.modify_subnet_attribute(SubnetId=subnet_id, MapPublicIpOnLaunch={'Value': True})
        return subnet_id
    except ec2_client.exceptions.ClientError as e:
        print(f"Error creating subnet: {e}")
        return None


