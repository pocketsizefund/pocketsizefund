package main

import (
	"encoding/json"
	"time"

	"github.com/pulumi/pulumi-awsx/sdk/go/awsx/ec2"
	"github.com/pulumi/pulumi-eks/sdk/v2/go/eks"
	"github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes"
	corev1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/core/v1"
	helmv3 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/helm/v3"
	metav1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/meta/v1"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi/config"
	"golang.org/x/exp/rand"
)

func generatePassword(length int) string {
	source := rand.NewSource(uint64(time.Now().UnixNano()))
	r := rand.New(source)

	chars := []rune("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
		"abcdefghijklmnopqrstuvwxyz" +
		"0123456789" +
		"!@#$%^&*()_+")
	password := make([]rune, length)
	for i := range password {
		password[i] = chars[r.Intn(len(chars))]
	}
	return string(password)
}

var namespaces = []string{"development", "paper", "live", "kubeflow", "monitoring"}

func main() {
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
			eksNodeInstanceType = "t3.small"
		}
		vpcNetworkCidr, err := cfg.Try("vpcNetworkCidr")
		if err != nil {
			vpcNetworkCidr = "10.0.0.0/16"
		}

		grafanaConfig := config.New(ctx, "grafana")
		grafanaVersion, err := grafanaConfig.Try("version")
		if err != nil {
			grafanaVersion = "8.3.6"
		}

		eksVPC, err := ec2.NewVpc(ctx, "eks-vpc", &ec2.VpcArgs{
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
			VpcId:                        eksVPC.VpcId,
			PublicSubnetIds:              eksVPC.PublicSubnetIds,
			PrivateSubnetIds:             eksVPC.PrivateSubnetIds,
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

		grafanaPassword := generatePassword(16)
		_, err = helmv3.NewRelease(ctx, "grafana", &helmv3.ReleaseArgs{
			Chart: pulumi.String("grafana"),
			// TODO versions should go in yaml
			Version:   pulumi.String(grafanaVersion), // pulumi.String("8.3.6"),
			Namespace: pulumi.String("monitoring"),
			RepositoryOpts: helmv3.RepositoryOptsArgs{
				Repo: pulumi.String("https://grafana.github.io/helm-charts"),
			},
			Values: pulumi.Map{
				"service": pulumi.Map{
					"type": pulumi.String("LoadBalancer"),
				},
				"adminPassword": pulumi.String(grafanaPassword),
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		_, err = helmv3.NewRelease(ctx, "prometheus", &helmv3.ReleaseArgs{
			Chart:     pulumi.String("kube-prometheus-stack"),
			Version:   pulumi.String("61.3.2"),
			Namespace: pulumi.String("monitoring"),
			RepositoryOpts: helmv3.RepositoryOptsArgs{
				Repo: pulumi.String("https://prometheus-community.github.io/helm-charts"),
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		_, err = helmv3.NewRelease(ctx, "promtail", &helmv3.ReleaseArgs{
			Chart:     pulumi.String("promtail"),
			Version:   pulumi.String("6.16.4"),
			Namespace: pulumi.String("monitoring"),
			RepositoryOpts: helmv3.RepositoryOptsArgs{
				Repo: pulumi.String("https://grafana.github.io/helm-charts"),
			},
			Values: pulumi.Map{
				"config": pulumi.Map{
					"lokiAddress": pulumi.String("http://loki:3100/loki/api/v1/push"),
				},
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		ctx.Export("kubeconfig", eksCluster.Kubeconfig)
		ctx.Export("vpcId", eksVPC.VpcId)
		ctx.Export("grafanaAdminPassword", pulumi.String(grafanaPassword))

		return nil
	})
}
