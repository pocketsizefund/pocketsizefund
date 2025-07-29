#!/usr/bin/env nu

def main [
    --dashboard-file: string = "grafana-dashboard.json"
    --workspace-id: string = ""
    --region: string = "us-east-1"
    --profile: string = "pulumi"
    --dry-run = false
] {
    check_prerequisites
    
    print "Pocket Size Fund Grafana Dashboard Upload"
    print "================================================"

    if not ($dashboard_file | path exists) {
        print $"Dashboard file not found: ($dashboard_file)"
        exit 1
    }

    print $"Using dashboard file: ($dashboard_file)"

    let workspace_id = if ($workspace_id | is-empty) {
        print "Getting Grafana workspace ID from AWS..."
        try {
            let result = (aws grafana list-workspaces
                --region $region
                --profile $profile
                --query 'workspaces[?name==`pocketsizefund`]'
                --output json
            | from json | get 0.id )
            if ($result | is-empty) {
                print "No workspace found with name 'pocketsizefund'"
                exit 1
            }
            $result
        } catch {
            print "Failed to get workspace ID. Provide --workspace-id or ensure workspace exists"
            exit 1
        }
    } else {
        $workspace_id
    }

    if ($workspace_id | is-empty) {
        print "No Grafana workspace found named 'pocketsizefund'"
        print "Create workspace first or provide specific --workspace-id"
        exit 1
    }

    print $"Target workspace ID: ($workspace_id)"

    print "Getting Grafana workspace endpoint..."
    let workspace_info = try {
        (aws grafana describe-workspace
            --workspace-id $workspace_id 
            --region $region 
            --profile $profile 
            --output json 
        | from json)
    } catch {
        print $"Failed to describe workspace: ($workspace_id)"
        exit 1
    }

    let grafana_endpoint = $workspace_info.workspace.endpoint
    print $"Grafana endpoint: ($grafana_endpoint)"

    print "Loading dashboard configuration..."
    let dashboard_content = try {
        open $dashboard_file | from json
    } catch {
        print $"Invalid JSON in dashboard file: ($dashboard_file)"
        exit 1
    }

    let upload_payload = {
        dashboard: $dashboard_content.dashboard
        overwrite: true
        message: $"Uploaded via Nu script at (date now | format date '%Y-%m-%d %H:%M:%S')"
    }

    if $dry_run {
        print "DRY RUN MODE - Dashboard payload preview:"
        print ($upload_payload | to json)
        print "\nDry run completed. Use without --dry-run to upload."
        exit 0
    }

    print "Creating Grafana API key..."
    let api_key_response = try {
        (aws grafana create-workspace-api-key 
            --workspace-id $workspace_id 
            --key-name $"pocketsizefund-upload-($workspace_id)-(date now | format date '%Y%m%d-%H%M%S')" 
            --key-role ADMIN 
            --seconds-to-live 3600 
            --region $region 
            --profile $profile 
            --output json 
        | from json)
    } catch {
        print "Failed to create API key. Check permissions."
        exit 1
    }

    let api_key = $api_key_response.key
    print "API key created successfully"

    print "Uploading dashboard to Grafana..."
    let upload_result = try {
        (http post $"($grafana_endpoint)/api/dashboards/db" 
            -H [
                "Authorization" $"Bearer ($api_key)"
                "Content-Type" "application/json"
            ] 
            ($upload_payload | to json))
    } catch {
        print "Failed to upload dashboard"

        try {
            (aws grafana delete-workspace-api-key 
                --workspace-id $workspace_id 
                --key-name $api_key_response.keyName 
                --region $region 
                --profile $profile 
            | ignore)
        } catch {
            # Ignore cleanup errors
        }
        
        exit 1
    }

    let result = try {
        $upload_result | from json
    } catch {
        print "Upload may have succeeded but response parsing failed"
        print $"Response: ($upload_result)"
    }

    if ($result | get status? | default "unknown") == "success" {
        print "Dashboard uploaded successfully!"
        print $"Dashboard URL: ($grafana_endpoint)/d/($result.slug)"
        print $"Dashboard ID: ($result.id)"
        print $"Version: ($result.version)"
    } else {
        print "Upload completed with unknown status"
        print $"Response: ($result)"
    }

    print "Cleaning up temporary API key..."
    try {
        (aws grafana delete-workspace-api-key 
            --workspace-id $workspace_id 
            --key-name $api_key_response.keyName 
            --region $region 
            --profile $profile 
        | ignore)
        print "API key cleaned up"
    } catch {
        print "Failed to clean up API key (manual deletion may be needed)"
    }

    print "\nDashboard upload completed!"
    print $"Access your dashboard at: ($grafana_endpoint)"
}

def check_prerequisites [] {
    print "Checking prerequisites..."
    
    try {
        aws --version | ignore
        print "AWS CLI available"
    } catch {
        print "AWS CLI not found. Please install AWS CLI."
        exit 1
    }

    try {
        which jq | ignore
        print "jq available"
    } catch {
        print "jq not found (optional but recommended for JSON debugging)"
    }

    print "Prerequisites check completed\n"
}