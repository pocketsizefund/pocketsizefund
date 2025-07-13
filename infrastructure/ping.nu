use std/assert

let cluster_endpoint = kubectl config view --minify --output jsonpath='{.clusters[0].cluster.server}'

let token = aws eks get-token --cluster-name pocketsizefund-cluster | from json | get status.token

let headers = [Authorization $"Bearer ($token)"]

let services = [
  {
    name: "datamanager"
    url: $"($cluster_endpoint)/api/v1/namespaces/default/services/datamanager:8080/proxy"
  }
  {
    name: "positionmanager"
    url: $"($cluster_endpoint)/api/v1/namespaces/default/services/positionmanager:8080/proxy"
  }
  {
    name: "predictionengine"
    url: $"($cluster_endpoint)/api/v1/namespaces/default/services/predictionengine:8080/proxy"
  }
]

$services
| each {|service|
  http get --full --headers $headers $"($service.url)/health"
  print $"($service.name) healthy"
}

let datamanager_url: string = ($services | where name == "datamanager" | get url | first)

let datamanager_get = http get --headers $headers $"($datamanager_url)/equity-bars?date=2025-01-07" | from json

assert (($datamanager_get | get count) >= 100)

let datamanager_query = {
  scheme: https
  host: ($cluster_endpoint | str replace "https://" "")
  path: "/api/v1/namespaces/default/services/datamanager:8080/proxy/equity-bars"
  params: {start_date: "2025-01-07" end_date: "2025-01-09"}
}
| url join
| http get --full --headers $headers $in
| get status

assert equal $datamanager_query 200

