import pulumi

common_tags = {
    "project": "pocketsizefund",
    "manager": "pulumi",
    "stack": pulumi.get_stack(),
}
