#!/usr/env/bin nu
use std/assert

fd .
  | find "Dockerfile"
  | path dirname
  | uniq
  | each {|service|
    let dockerfile_version = open $"($service)/Dockerfile"
      | find "FROM python"
      | $in.0
      | str replace --regex '.*:(\d+\.\d+)-.*' '$1'
    let pyproject_version = open $"($service)/pyproject.toml"
      | get project.requires-python

    assert ($pyproject_version starts-with "==") $"pyproject python version must be pinned with \"==\", got: ($pyproject_version)"
    let pyproject_version = $pyproject_version | str replace "==" ""

    assert ($dockerfile_version == $pyproject_version) $"dockerfile version [($dockerfile_version)] does not match pyproject.toml version [($pyproject_version)]"
    
    {
      service: $service
      docker_version: $dockerfile_version
      python_version: $pyproject_version
    }
  }
