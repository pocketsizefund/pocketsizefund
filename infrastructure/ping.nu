use std/assert

let headers = [Authorization $"Bearer (gcloud auth print-identity-token)"]
let services = gcloud run services list --format=json
| from json
| get status.address.url
| each {|url|
  {
    name: ($url | split row "https://" | get 1 | split row "-" | get 0)
    url: $url
  }
}

$services
| each {|service|
  http get --full --headers $headers $"($service.url)/health"
  print $"($service.name) healthy"
}


let datamanager_url: string = ($services | where name == "datamanager" | get url.0)


let datamanager_post = {date: "2025-01-07"}
| to json
| http post --headers $headers $"($datamanager_url)/equity-bars" 


assert (($datamanager_post | get count) >= 100)

let datamanager_fetch = {
  scheme: https
  host: ($datamanager_url | str replace "https://" "")
  path: "/equity-bars"
  params: {start_date: "2025-01-07" end_date: "2025-01-09"}
}
| url join
| http get --full --headers $headers $in
| get status

assert equal $datamanager_fetch 200

