use hcl::Body;
use std::collections::HashMap;

use std::io::{self, Write};
use std::process::Command;
use termion::cursor::Goto;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

// use nexus7::aws::availability_zone::AvailabilityZone;
// use nexus7::aws::eks::Cluster;
// use nexus7::aws::iam;
// use nexus7::aws::network::cidr;
// use nexus7::aws::network::gateway;
// use nexus7::aws::network::subnet::Subnet;
// use nexus7::aws::network::vpc;
use nexus7::aws::provider::AwsProvider;
use nexus7::aws::region::Region;
use nexus7::aws::storage::s3;
use nexus7::aws::storage::s3::bucket::Bucket;
use nexus7::tags::{Tag, Tags};
use std::net::Ipv4Addr;

// fn create_stack() -> Result<Body, Box<dyn std::error::Error>> {


//     let vpc = vpc::Vpc {
//         name: "eks-vpc".to_string(),
//         cidr_block: cidr::Block::new(Ipv4Addr::new(10, 0, 0, 0), 16)?,
//         instance_tenancy: Some("default".to_string()),
//         enable_dns_hostnames: Some(true),
//         enable_dns_support: Some(true),
//         enable_classiclink: None,
//         enable_classiclink_dns_support: None,
//         assign_generated_ipv6_cidr_block: Some(false),
//         tags: Some(HashMap::from([(
//             "environment".to_string(),
//             "live".to_string(),
//         )])),
//     };

//     let eip = vpc::ElasticIp {
//         name: "nat-gateway-eip".to_string(),
//         domain: Some("vpc".to_string()),
//         instance: None,
//         network_interface: None,
//         public_ipv4_pool: None,
//         customer_owned_ipv4_pool: None,
//         associate_with_private_ip: None,
//         address: None,
//         tags: Some(HashMap::from([(
//             "Name".to_string(),
//             "NAT Gateway EIP".to_string(),
//         )])),
//     };

//     let internet_gateway = gateway::Internet {
//         name: "vpc-internet-gateway".to_string(),
//         vpc: &vpc.clone(),
//         tags: None,
//     };

//     let subnet1 = Subnet {
//         name: "subnet1".to_string(),
//         vpc: &vpc.clone(),
//         cidr_block: cidr::Block::new(Ipv4Addr::new(10, 0, 0, 0), 24)?,
//         assign_ipv6_address_on_creation: None,
//         availability_zone: Some(AvailabilityZone::UsEast1a),
//         ipv6_cidr_block: None,
//         map_public_ip_on_launch: None,
//         tags: None,
//     };

//     let subnet2 = Subnet {
//         name: "subnet2".to_string(),
//         vpc: &vpc.clone(),
//         cidr_block: cidr::Block::new(Ipv4Addr::new(10, 0, 1, 0), 24)?,
//         assign_ipv6_address_on_creation: None,
//         availability_zone: Some(AvailabilityZone::UsEast1b),
//         ipv6_cidr_block: None,
//         map_public_ip_on_launch: None,
//         tags: None,
//     };

//     let subnet3 = Subnet {
//         name: "subnet3".to_string(),
//         vpc: &vpc.clone(),
//         cidr_block: cidr::Block::new(Ipv4Addr::new(10, 0, 2, 0), 24)?,
//         assign_ipv6_address_on_creation: None,
//         availability_zone: Some(AvailabilityZone::UsEast1c),
//         ipv6_cidr_block: None,
//         map_public_ip_on_launch: None,
//         tags: None,
//     };

//     let nat = gateway::NAT {
//         id: None,
//         vpc: &vpc.clone(),
//         elastic_ip: &eip.clone(),
//         connectivity_type: None,
//         state: None,
//         subnet: &subnet1.clone(),
//         tags: None,
//     };

    // let cluster = Cluster {
    //     name: "pocketsizefund-eks-cluster".to_string(),
    //     role: &iam::Role {
    //         arn: "arn:aws:iam::123456789012:role/eks-cluster-role".to_string(),
    //     },
    //     vpc: &vpc.clone(),
    //     enabled_cluster_log_types: None,
    //     encryption_config: None,
    //     endpoint_private_access: None,
    //     endpoint_public_access: None,
    //     version: None,
    //     kubernetes_version: None,
    //     subnet_ids: vec![&subnet1.clone(), &subnet2.clone(), &subnet3.clone()],
    //     tags: None,
    // };

//     
//         .add_block(provider)
//         .add_block(bucket)
//         // .add_block(vpc)
//         // .add_block(internet_gateway)
//         // .add_block(eip)
//         // .add_block(nat)
//         // .add_block(subnet1)
//         // .add_block(subnet2)
//         // .add_block(subnet3)
//         // .add_block(cluster)
//         .build();

//     Ok(body)
// }

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let tags = Tags::new(vec![Tag::new("environment".to_string(), "live".to_string())]);

    let provider = AwsProvider {
        region: Region::UsEast1,
    };

    let bucket = Bucket::new("pocketsizefund-unique-bucket-name".to_string())
        .with_force_destroy(true)
        .with_acl(s3::acl::Options::Private)
        .with_tags(tags);

    let body = Body::builder()
        .add_block(provider)
        .add_block(bucket)
        .build();

    let hcl = hcl::to_string(&body).unwrap();
    // TODO: this is a hack to fix the issue of the double dollar sign
    let corrected_hcl = hcl.replace(" = \"$$", " = \"$");

    std::fs::write("main.tf", corrected_hcl)?;

    println!("HCL file 'main.tf' has been generated.");

    let mut stdout = io::stdout().into_raw_mode()?;
    let stdin = io::stdin();
    let mut stdin = stdin.keys();

    let options = ["Up (init, plan, apply)", "Down (destroy)"];
    let mut selected = 0;

    loop {
        write!(stdout, "{}{}", termion::clear::All, Goto(1, 1))?;
        write!(stdout, "Use j/k to navigate, Enter to select:")?;
        for (i, option) in options.iter().enumerate() {
            if i == selected {
                write!(stdout, "\n\r> {}", option)?;
            } else {
                write!(stdout, "\n\r  {}", option)?;
            }
        }
        stdout.flush()?;

        if let Some(Ok(key)) = stdin.next() {
            match key {
                Key::Char('j') => selected = (selected + 1) % options.len(),
                Key::Char('k') => selected = (selected + options.len() - 1) % options.len(),
                Key::Char('\n') => break,
                Key::Ctrl('c') => return Ok(()),
                _ => {}
            }
        }
    }

    write!(stdout, "{}", termion::clear::All)?;
    stdout.suspend_raw_mode()?;

    match selected {
        0 => {
            println!("Running OpenTofu init...");
            let output = Command::new("tofu").current_dir(".").arg("init").output()?;
            if output.status.success() {
                println!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            }

            println!("Running OpenTofu plan...");
            let output = Command::new("tofu").current_dir(".").arg("plan").output()?;
            if output.status.success() {
                println!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            }

            println!("Do you want to apply these changes? (y/n)");
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            if input.trim().to_lowercase() == "y" {
                println!("Running OpenTofu apply...");
                let output = Command::new("tofu")
                    .current_dir(".")
                    .arg("apply")
                    .arg("-auto-approve")
                    .output()?;
                if output.status.success() {
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                } else {
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                }
            }
        }
        1 => {
            println!("Do you want to destroy the resources? (y/n)");
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            if input.trim().to_lowercase() == "y" {
                println!("Running OpenTofu destroy...");
                let output = Command::new("tofu")
                    .current_dir(".")
                    .arg("destroy")
                    .arg("-auto-approve")
                    .output()?;
                if output.status.success() {
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                } else {
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                }
            }
        }
        _ => {}
    }

    Ok(())
}
