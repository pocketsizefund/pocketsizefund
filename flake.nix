{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    nixpkgs-python = {
      url = "github:cachix/nixpkgs-python";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    systems,
    ...
  } @ inputs: let
    forEachSystem = nixpkgs.lib.genAttrs (import systems);
  in {
    packages = forEachSystem (system: {
      devenv-up = self.devShells.${system}.default.config.procfileScript;
    });

    devShells =
      forEachSystem
      (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({
              pkgs,
              config,
              ...
            }: {
              packages = with pkgs; [
                git
                alejandra
                nushell
                cargo-deny
                openssl
                mise
                ruff
                awscli
                jq
                helm
                yq
                eksctl
                kubectl
                krew
                kn
                gum
              ];

              languages = {
                nix.enable = true;
                rust = {
                  enable = true;
                  channel = "stable";
                  components = [
                    "rustc"
                    "cargo"
                    "clippy"
                    "rustfmt"
                    "rust-analyzer"
                  ];
                };
                python = {
                  enable = true;
                  version = "3.12.4";
                  uv = {
                    enable = true;
                    sync = {
                      enable = true;
                      allExtras = true;
                    };
                  };
                };
              };

              enterShell = ''
              '';

              tasks = {
                "nix:lint" = {
                  exec = "alejandra .";
                  before = ["devenv:enterShell"];
                };
                "k8s:kubectl:krew:install" = {
                  exec = ''
                    krew install krew
                    kubectl krew install assert
                    kubectl krew install colorize-applied
                    kubectl krew install cost
                  '';
                  before = ["k8s:costs"];
                };
                "k8s:costs" = {
                  exec = ''
                    kubectl cost namespace --show-all-resources
                  '';
                  before = ["devenv:enterShell"];
                };
                # "cargo:clippy" = {
                #   exec = "cargo clippy --allow-dirty --fix";
                #   before = ["devenv:enterShell"];
                # };
                # "cargo:test" = {
                #   exec = "cargo test --all-features";
                #   before = ["devenv:enterShell"];
                # };
              };
            })
          ];
        };
      });
  };
}
