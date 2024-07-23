package main

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/pulumi/pulumi-aws/sdk/v5/go/aws/rds"
	"github.com/pulumi/pulumi-aws/sdk/v5/go/aws/s3"
	"github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes"
	corev1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/core/v1"
	helm "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/helm/v3"
	metav1 "github.com/pulumi/pulumi-kubernetes/sdk/v3/go/kubernetes/meta/v1"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi/config"
)

func main() {
	pulumi.Run(func(ctx *pulumi.Context) error {
		cfg := config.New(ctx, "")
		dbUsername := cfg.RequireSecret("dbUsername")
		dbPassword := cfg.RequireSecret("dbPassword")

		argocdConfig := config.New(ctx, "argocd")
		argocdAdminPassword := argocdConfig.RequireSecret("adminPassword")

		homeDir, err := os.UserHomeDir()
		if err != nil {
			return err
		}
		kubeconfigPath := filepath.Join(homeDir, ".kube", "config")

		kubeconfig, err := os.ReadFile(kubeconfigPath)
		if err != nil {
			return err
		}

		k8sProvider, err := kubernetes.NewProvider(ctx, "k8s-provider", &kubernetes.ProviderArgs{
			Kubeconfig: pulumi.String(string(kubeconfig)),
		})
		if err != nil {
			return err
		}

		namespaces := []string{"istio-system", "cert-manager", "kyverno", "argocd"}
		for _, ns := range namespaces {
			_, err := corev1.NewNamespace(ctx, ns, &corev1.NamespaceArgs{
				Metadata: &metav1.ObjectMetaArgs{
					Name: pulumi.String(ns),
				},
			}, pulumi.Provider(k8sProvider))
			if err != nil {
				return err
			}
		}

		istioBase, err := helm.NewRelease(ctx, "istio", &helm.ReleaseArgs{
			Chart:     pulumi.String("base"),
			Version:   pulumi.String("1.22.3"),
			Namespace: pulumi.String("istio-system"),
			RepositoryOpts: helm.RepositoryOptsArgs{
				Repo: pulumi.String("https://istio-release.storage.googleapis.com/charts"),
			},
			Timeout:     pulumi.Int(900), // 15 minutes timeout
			WaitForJobs: pulumi.Bool(true),
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		maxRetries := 3
		for i := 0; i < maxRetries; i++ {
			_, err = helm.NewRelease(ctx, "istiod", &helm.ReleaseArgs{
				Chart:     pulumi.String("istiod"),
				Version:   pulumi.String("1.22.3"),
				Namespace: pulumi.String("istio-system"),
				RepositoryOpts: helm.RepositoryOptsArgs{
					Repo: pulumi.String("https://istio-release.storage.googleapis.com/charts"),
				},
				Timeout:     pulumi.Int(900), // 15 minutes timeout
				WaitForJobs: pulumi.Bool(true),
				Values: pulumi.Map{
					"global": pulumi.Map{
						"proxy": pulumi.Map{
							"autoInject": pulumi.String("disabled"),
						},
					},
				},
			}, pulumi.Provider(k8sProvider),
				pulumi.DependsOn([]pulumi.Resource{istioBase}),
				pulumi.DeleteBeforeReplace(true),
				pulumi.RetainOnDelete(true))

			if err == nil {
				break
			}

			if i < maxRetries-1 {
				ctx.Log.Warn(fmt.Sprintf("Istiod installation failed, retrying in 30 seconds (attempt %d of %d)", i+1, maxRetries), nil)
				time.Sleep(30 * time.Second)
			} else {
				return fmt.Errorf("failed to install Istiod after %d attempts: %v", maxRetries, err)
			}
		}

		_, err = helm.NewRelease(ctx, "cert-manager", &helm.ReleaseArgs{
			Chart:     pulumi.String("cert-manager"),
			Version:   pulumi.String("v1.13.1"),
			Namespace: pulumi.String("cert-manager"),
			RepositoryOpts: helm.RepositoryOptsArgs{
				Repo: pulumi.String("https://charts.jetstack.io"),
			},
			Values: pulumi.Map{
				"installCRDs": pulumi.Bool(true),
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		_, err = helm.NewRelease(ctx, "kyverno", &helm.ReleaseArgs{
			Chart:     pulumi.String("kyverno"),
			Version:   pulumi.String("3.2.6"),
			Namespace: pulumi.String("kyverno"),
			RepositoryOpts: helm.RepositoryOptsArgs{
				Repo: pulumi.String("https://kyverno.github.io/kyverno/"),
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		_, err = helm.NewRelease(ctx, "argocd", &helm.ReleaseArgs{
			Chart:     pulumi.String("argo-cd"),
			Version:   pulumi.String("7.3.10"),
			Namespace: pulumi.String("argocd"),
			RepositoryOpts: helm.RepositoryOptsArgs{
				Repo: pulumi.String("https://argoproj.github.io/argo-helm"),
			},
			Values: pulumi.Map{
				"server": pulumi.Map{
					"extraArgs": pulumi.Array{
						// TODO we probalby want TLS
						pulumi.String("--insecure"),
					},
				},
				"configs": pulumi.Map{
					"secret": pulumi.Map{
						"argocdServerAdminPassword": argocdAdminPassword,
					},
				},
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		db, err := rds.NewInstance(ctx, "kubeflow-mysql", &rds.InstanceArgs{
			Engine:             pulumi.String("mysql"),
			EngineVersion:      pulumi.String("8.0"),
			InstanceClass:      pulumi.String("db.t3.micro"),
			AllocatedStorage:   pulumi.Int(20),
			Name:               pulumi.String("kubeflow"),
			Username:           dbUsername,
			Password:           dbPassword,
			SkipFinalSnapshot:  pulumi.Bool(true),
			PubliclyAccessible: pulumi.Bool(false),
		})
		if err != nil {
			return err
		}

		bucket, err := s3.NewBucket(ctx, "kubeflow-storage", &s3.BucketArgs{
			Bucket: pulumi.String("psf-kubeflow-artifacts"),
		})
		if err != nil {
			return err
		}

		_, err = helm.NewRelease(ctx, "kubeflow", &helm.ReleaseArgs{
			Chart:     pulumi.String("kubeflow"),
			Version:   pulumi.String("1.8.0"),
			Namespace: pulumi.String("kubeflow"),
			RepositoryOpts: helm.RepositoryOptsArgs{
				Repo: pulumi.String("https://github.com/kubeflow/kubeflow/releases/tag/v1.9.0"),
			},
			Values: pulumi.Map{
				"profile-controller": pulumi.Map{
					"enabled": pulumi.Bool(true),
					"mysql": pulumi.Map{
						"host":     db.Endpoint,
						"port":     db.Port,
						"username": dbUsername,
						"password": dbPassword,
					},
					"objectStore": pulumi.Map{
						"bucket": bucket.ID(),
						"region": pulumi.String(awsRegion),
					},
				},
			},
		}, pulumi.Provider(k8sProvider))
		if err != nil {
			return err
		}

		ctx.Export("rdsEndpoint", db.Endpoint)
		ctx.Export("s3BucketName", bucket.ID())

		return nil
	})
}
