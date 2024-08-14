package main

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/apiextensions"
	rbacv1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/rbac/v1"

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

var namespaces = []string{"live", "kubeflow", "monitoring"}

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

		grafanaConfig := config.New(ctx, "grafana")
		grafanaVersion, err := grafanaConfig.Try("version")
		if err != nil {
			grafanaVersion = "8.3.6"
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

		namespaceMap := make(map[string]*corev1.Namespace)
		for _, namespaceName := range namespaces {
			ns, err := corev1.NewNamespace(ctx, namespaceName, &corev1.NamespaceArgs{
				Metadata: &metav1.ObjectMetaArgs{
					Name: pulumi.String(namespaceName),
				},
			}, pulumi.Provider(k8sProvider))
			if err != nil {
				return err
			}
			namespaceMap[namespaceName] = ns
		}

		grafanaPassword := generatePassword(16)
		const maxRetries = 3

		for i := 0; i < maxRetries; i++ {
			_, err = helmv3.NewRelease(ctx, "grafana", &helmv3.ReleaseArgs{
				Chart:     pulumi.String("grafana"),
				Version:   pulumi.String(grafanaVersion),
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
				WaitForJobs: pulumi.Bool(true),
			}, pulumi.Provider(k8sProvider), pulumi.DeleteBeforeReplace(true), pulumi.DependsOn([]pulumi.Resource{namespaceMap["monitoring"]}))

			if err == nil {
				break
			}

			if i < maxRetries-1 {
				time.Sleep(10 * time.Second)
			}
		}

		if err != nil {
			return fmt.Errorf("failed to create Grafana release after %d attempts: %v", maxRetries, err)
		}

		_, err = helmv3.NewRelease(ctx, "prometheus", &helmv3.ReleaseArgs{
			Chart:     pulumi.String("kube-prometheus-stack"),
			Version:   pulumi.String("61.3.2"),
			Namespace: pulumi.String("monitoring"),
			RepositoryOpts: helmv3.RepositoryOptsArgs{
				Repo: pulumi.String("https://prometheus-community.github.io/helm-charts"),
			},
		}, pulumi.Provider(k8sProvider), pulumi.DependsOn([]pulumi.Resource{namespaceMap["monitoring"]}))
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
		}, pulumi.Provider(k8sProvider), pulumi.DependsOn([]pulumi.Resource{namespaceMap["monitoring"]}))
		if err != nil {
			return err
		}

		_, err = apiextensions.NewCustomResource(ctx, "knative-eventing", &apiextensions.CustomResourceArgs{
			ApiVersion: pulumi.String("operator.knative.dev/v1alpha1"),
			Kind:       pulumi.String("KnativeEventing"),
			Metadata: &metav1.ObjectMetaArgs{
				Name:      pulumi.String("knative-eventing"),
				Namespace: pulumi.String("knative-eventing"),
			},
		}, pulumi.Provider(k8sProvider), pulumi.DependsOn([]pulumi.Resource{namespaceMap["live"]}))
		if err != nil {
			return err
		}

		_, err = apiextensions.NewCustomResource(ctx, "default-broker", &apiextensions.CustomResourceArgs{
			ApiVersion: pulumi.String("eventing.knative.dev/v1"),
			Kind:       pulumi.String("Broker"),
			Metadata: &metav1.ObjectMetaArgs{
				Name:      pulumi.String("default"),
				Namespace: pulumi.String("default"),
			},
		}, pulumi.Provider(k8sProvider), pulumi.DependsOn([]pulumi.Resource{namespaceMap["live"]}))
		if err != nil {
			return err
		}

		_, err = rbacv1.NewClusterRole(ctx, "knative-eventing-controller", &rbacv1.ClusterRoleArgs{
			Metadata: &metav1.ObjectMetaArgs{
				Name: pulumi.String("knative-eventing-controller"),
				Annotations: pulumi.StringMap{
					"pulumi.com/patchForce": pulumi.String("true"),
				},
			},
			Rules: rbacv1.PolicyRuleArray{
				&rbacv1.PolicyRuleArgs{
					ApiGroups: pulumi.StringArray{
						pulumi.String("sources.knative.dev"),
						pulumi.String("messaging.knative.dev"),
						pulumi.String("flows.knative.dev"),
						pulumi.String("apiextensions.k8s.io"),
					},
					Resources: pulumi.StringArray{
						pulumi.String("apiserversources"),
						pulumi.String("pingsources"),
						pulumi.String("channels"),
						pulumi.String("subscriptions"),
						pulumi.String("parallels"),
						pulumi.String("customresourcedefinitions"),
					},
					Verbs: pulumi.StringArray{
						pulumi.String("get"),
						pulumi.String("list"),
						pulumi.String("watch"),
					},
				},
			},
		}, pulumi.Provider(k8sProvider), pulumi.DeleteBeforeReplace(true))
		if err != nil {
			return err
		}

		_, err = rbacv1.NewClusterRoleBinding(ctx, "knative-eventing-controller-binding", &rbacv1.ClusterRoleBindingArgs{
			Metadata: &metav1.ObjectMetaArgs{
				Name: pulumi.String("knative-eventing-controller-binding"),
			},
			RoleRef: &rbacv1.RoleRefArgs{
				ApiGroup: pulumi.String("rbac.authorization.k8s.io"),
				Kind:     pulumi.String("ClusterRole"),
				Name:     pulumi.String("knative-eventing-controller"),
			},
			Subjects: rbacv1.SubjectArray{
				&rbacv1.SubjectArgs{
					Kind:      pulumi.String("ServiceAccount"),
					Name:      pulumi.String("eventing-controller"),
					Namespace: pulumi.String("live"),
				},
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		alpacaConfig := config.New(ctx, "alpaca")
		darqubeConfig := config.New(ctx, "darqube")
		wandbConfig := config.New(ctx, "wandb")
		alphaVantagConfig := config.New(ctx, "alphavantage")
		edgarConfig := config.New(ctx, "edgar")
		sentryConfig := config.New(ctx, "sentry")
		discordConfig := config.New(ctx, "discord")
		claudeConfig := config.New(ctx, "claude")

		_, err = corev1.NewSecret(ctx, "platform-secret-live", &corev1.SecretArgs{
			Metadata: &metav1.ObjectMetaArgs{
				Name:      pulumi.String("platform"),
				Namespace: pulumi.String("live"),
			},
			StringData: pulumi.StringMap{
				"APCA_API_KEY_ID":       alpacaConfig.RequireSecret("api-key-id"),
				"APCA_API_SECRET_KEY":   alpacaConfig.RequireSecret("api-secret-key"),
				"ALPACA_API_KEY_ID":     alpacaConfig.RequireSecret("api-key-id"),
				"ALPACA_API_SECRET_KEY": alpacaConfig.RequireSecret("api-secret-key"),
				"DARQUBE_API_KEY":       darqubeConfig.RequireSecret("api-key"),
				"WANDB_API_KEY":         wandbConfig.RequireSecret("api-key"),
				"ALPHA_VANTAGE_API_KEY": alphaVantagConfig.RequireSecret("api-key"),
				"EDGAR_USER_AGENT":      edgarConfig.RequireSecret("user-agent"),
				"FUND_ENVIRONMENT":      pulumi.String("live"),
				"MODEL_FILE_NAME":       pulumi.String("price-model.ckpt"),
				"SENTRY_DSN":            sentryConfig.RequireSecret("dsn"),
				"DISCORD_TOKEN":         discordConfig.RequireSecret("token"),
				"DISCORD_WEBHOOK_URL":   discordConfig.RequireSecret("webhook-url"),
				"CLAUDE_API_KEY":        claudeConfig.RequireSecret("api-key"),
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
