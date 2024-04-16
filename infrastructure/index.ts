import * as pulumi from "@pulumi/pulumi";
import * as aws from "@pulumi/aws";
import * as awsx from "@pulumi/awsx";
import * as docker from "@pulumi/docker";

const projectName: string = "pocketsizefund";

const dataBucket = new aws.s3.Bucket(`${projectName}-data`);
const stackArtifactsBucket = new aws.s3.Bucket(`${projectName}-stack-artifacts`);
const mlArtifactsBucket = new aws.s3.Bucket(`${projectName}-ml-artifacts`);


const loadbalancer = new awsx.lb.ApplicationLoadBalancer("loadbalancer", {
});

const containerRegistry = new awsx.ecr.Repository("container-registry", {
	name: "image-container-registry",
	imageTagMutability: "IMMUTABLE",
	imageScanningConfiguration: {
		scanOnPush: true,
	},
})

const image = new docker.Image("example-image", {
	// imageName: containerRegistry.repositoryUrl,
	repositoryUrl: containerRegistry.repositoryUrl,
	build: {
		context: "./example-app",
		dockerfile: "Dockerfile",
	},
	registry: {
	  server: containerRegistry.repositoryUrl,
	}
});


const cluster = new aws.ecs.Cluster("cluster", {});



const fargate = new awsx.ecs.FargateService("service", {
	cluster: cluster.arn,
	assignPublicIp: true,
	taskDefinitionArgs: {
		container: {
			name: `${projectName}-ecs`,
			image: image.imageUri,
			cpu: 8,
			memory: 512,
			essential: true,
			portMappings: [{
				containerPort: 80,
				targetGroup: loadbalancer.defaultTargetGroup,
			}],
		},
	},
});


export const dataBucketName = dataBucket.id;
export const stackArtifactsBucketName = stackArtifactsBucket.id;
export const mlArtifactsBucketName = mlArtifactsBucket.id;
export const frontendUrl = pulumi.interpolate `http://${loadbalancer.loadBalancer.dnsName}`;
