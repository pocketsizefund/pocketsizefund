from invoke import task, Collection
from steps import aws, k8s

ns = Collection()

aws_ns = Collection("aws")
aws_ns.add_collection(aws.vpc, name="vpc")
aws_ns.add_collection(aws.eks, name="eks")
aws_ns.add_collection(aws.node_group, name="node_group")
aws_ns.add_collection(aws.subnet, name="subnet")
aws_ns.add_collection(aws.route_table, name="route_table")
aws_ns.add_collection(aws, name="*")
ns.add_collection(aws_ns)

k8s_ns = Collection("k8s")
k8s_ns.add_collection(k8s, name="*")
k8s_ns.add_collection(k8s.knative)
ns.add_collection(k8s_ns)

ns.configure({'run': {'echo': True}})
