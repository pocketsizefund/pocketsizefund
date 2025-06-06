let headers = [Authorization $"Bearer (gcloud auth print-identity-token)"]
let services = gcloud run services list --format=json
| from json
| get status.address.url
| each {|url|
  {
    service: ($url | split row "https://" | get 1 | split row "-" | get 0)
    url: $url
  }
}

services
| each {|service| http get --full --headers $headers $"($service)/health" }


let datamanager_url = ($services | where service == "datamanager" | get url.0)

{date: "2025-01-04"}
| to json
| http post --full --headers $headers $"($datamanager_url)/equity-bars" 
