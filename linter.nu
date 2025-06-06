#!/usr/bin/env nu
use std/assert

fd .
  | find "Dockerfile"
  | path dirname
  | uniq
  | each {|service|
    let dockerfile_version = if ($"($service)/Dockerfile" | path exists) {
      let from_lines = open $"($service)/Dockerfile" | find "FROM python"
      if ($from_lines | length) > 0 {
        $from_lines.0 | str replace --regex '.*python:(\d+\.\d+).*' '$1'
      } else {
        error make {msg: $"No 'FROM python' line found in ($service)/Dockerfile"}
      }
    } else {
      error make {msg: $"Dockerfile not found in ($service)"}
    }
    let pyproject_version = if ($"($service)/pyproject.toml" | path exists) {
      let toml_content = open $"($service)/pyproject.toml"
      if "project" in $toml_content and "requires-python" in $toml_content.project {
        $toml_content.project.requires-python
      } else {
        error make {msg: $"Missing 'project.requires-python' field in ($service)/pyproject.toml"}
      }
    } else {
      error make {msg: $"pyproject.toml not found in ($service)"}
    }

    assert ($pyproject_version starts-with "==") $"pyproject python version must be pinned with \"==\", got: ($pyproject_version)"
    let pyproject_version = $pyproject_version | str replace "==" ""

    assert ($dockerfile_version == $pyproject_version) $"dockerfile version [($dockerfile_version)] does not match pyproject.toml version [($pyproject_version)]"
    
    {
      service: $service
      docker_version: $dockerfile_version
      python_version: $pyproject_version
    }
  }
