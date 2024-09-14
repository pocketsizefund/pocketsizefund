FROM puppet/puppet
FROM puppet/puppet-bolt

COPY bolt-project.yaml .
COPY inventory.yaml .

ENTRYPOINT ["bolt", "plan"]
