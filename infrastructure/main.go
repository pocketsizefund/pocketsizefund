package main

import (
	"encoding/json"

	"github.com/pulumi/pulumi-awsx/sdk/go/awsx/ec2"
	"github.com/pulumi/pulumi-eks/sdk/v2/go/eks"
	"github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes"
	corev1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/core/v1"
	metav1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/meta/v1"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi/config"
)

func main() {
	namespaces := []string{"development", "paper", "live", "ml"}

	pulumi.Run(func(ctx *pulumi.Context) error {
		cfg := config.New(ctx, "")
		minClusterSize, err := cfg.TryInt("minClusterSize")
		if err != nil {
			minClusterSize = 3
		}
		maxClusterSize, err := cfg.TryInt("maxClusterSize")
		if err != nil {
			maxClusterSize = 6
		}
		desiredClusterSize, err := cfg.TryInt("desiredClusterSize")
		if err != nil {
			desiredClusterSize = 3
		}
		eksNodeInstanceType, err := cfg.Try("eksNodeInstanceType")
		if err != nil {
			eksNodeInstanceType = "t3.medium"
		}
		vpcNetworkCidr, err := cfg.Try("vpcNetworkCidr")
		if err != nil {
			vpcNetworkCidr = "10.0.0.0/16"
		}

		eksVpc, err := ec2.NewVpc(ctx, "eks-vpc", &ec2.VpcArgs{
			EnableDnsHostnames: pulumi.Bool(true),
			CidrBlock:          &vpcNetworkCidr,
			NatGateways: &ec2.NatGatewayConfigurationArgs{
				Strategy: ec2.NatGatewayStrategyOnePerAz,
			},
		})
		if err != nil {
			return err
		}

		eksCluster, err := eks.NewCluster(ctx, "eks-cluster", &eks.ClusterArgs{
			VpcId:                        eksVpc.VpcId,
			PublicSubnetIds:              eksVpc.PublicSubnetIds,
			PrivateSubnetIds:             eksVpc.PrivateSubnetIds,
			InstanceType:                 pulumi.String(eksNodeInstanceType),
			DesiredCapacity:              pulumi.Int(desiredClusterSize),
			MinSize:                      pulumi.Int(minClusterSize),
			MaxSize:                      pulumi.Int(maxClusterSize),
			NodeAssociatePublicIpAddress: pulumi.BoolRef(false),
			EndpointPrivateAccess:        pulumi.Bool(false),
			EndpointPublicAccess:         pulumi.Bool(true),
		})
		if err != nil {
			return err
		}

		k8sProvider, err := kubernetes.NewProvider(ctx, "k8s-provider", &kubernetes.ProviderArgs{
			Kubeconfig: eksCluster.Kubeconfig.ApplyT(func(kubeconfig interface{}) string {
				switch v := kubeconfig.(type) {
				case string:
					return v
				case map[string]interface{}:
					jsonBytes, err := json.Marshal(v)
					if err != nil {
						return ""
					}
					return string(jsonBytes)
				default:
					return ""
				}
			}).(pulumi.StringOutput),
		})
		if err != nil {
			return err
		}

		for _, namespace := range namespaces {
			_, err := corev1.NewNamespace(ctx, namespace, &corev1.NamespaceArgs{
				Metadata: &metav1.ObjectMetaArgs{
					Name: pulumi.String(namespace),
				},
			}, pulumi.Provider(k8sProvider))
			if err != nil {
				return err
			}

			_, err = corev1.NewSecret(ctx, "platform-secret-"+namespace, &corev1.SecretArgs{
				Metadata: &metav1.ObjectMetaArgs{
					Name:      pulumi.String("platform"),
					Namespace: pulumi.String(namespace),
				},
				StringData: pulumi.StringMap{
					"username": pulumi.String("admin"),
					"password": pulumi.String("supersecret"),
				},
			}, pulumi.Provider(k8sProvider))
			if err != nil {
				return err
			}
		}

		ctx.Export("kubeconfig", eksCluster.Kubeconfig)
		ctx.Export("vpcId", eksVpc.VpcId)
		return nil
	})
}
