#!/usr/bin/env nu

# upload Grafana dashboard to Grafana Cloud
# Usage: nu upload_grafana_dashboard.nu

let grafana_url = $env.GRAFANA_CLOUD_URL? | default ""
let grafana_api_key = $env.GRAFANA_API_KEY? | default ""

if ($grafana_api_key | is-empty) {
    print "GRAFANA_API_KEY environment variable is required"
    exit 1
}

if ($grafana_url == "") {
    print "GRAFANA_CLOUD_URL environment variable is required"
    exit 1
}

let dashboard_file = "grafana_dashboard.json"

if not ($dashboard_file | path exists) {
    print $"dashboard file '($dashboard_file)' not found"
    exit 1
}

let dashboard_content = open $dashboard_file | from json

let upload_payload = {
    dashboard: $dashboard_content.dashboard
    overwrite: true
    message: "uploaded via Nu script"
}

let headers = [
    "Authorization" $"Bearer ($grafana_api_key)"
    "Content-Type" "application/json"
]

try {
    let response = $upload_payload 
    | to json 
    | http post --headers $headers $"($grafana_url)/api/dashboards/db"
    
    print "dashboard uploaded successfully!"
    
} catch { |error|
    print $"failed to upload dashboard: ($error)"
}
