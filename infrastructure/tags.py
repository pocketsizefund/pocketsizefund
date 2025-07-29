import pulumi

pulumi_tags = {
    "project": "pocketsizefund",
    "manager": "pulumi",
    "stack": pulumi.get_stack(),
}

manual_tags = {
    "project": "pocketsizefund",
    "manager": "manual",
    "stack": "none",
}
