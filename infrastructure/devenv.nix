{pkgs, ...}: {
  env.GREET = "infrastructure console";

  packages = [
    pkgs.pulumi
    pkgs.gum
    pkgs.nodejs
    pkgs.pulumiPackages.pulumi-language-nodejs
  ];

  enterShell = ''
    echo "pulumi version"
    ${pkgs.pulumi}/bin/pulumi version
  '';

  enterTest = ''
  '';

  starship.enable = true;
  languages.nix.enable = true;
  languages.typescript.enable = true;

  scripts.init = {
    exec = ''
      AWS_PROFILE=$(${pkgs.gum}/bin/gum input --placeholder=AWS_PROFILE_NAME)
      export AWS_PROFILE=$AWS_PROFILE
      ${pkgs.pulumi}/bin/pulumi new aws-typescript --force
    '';
    description = "create a new pulumi stack with the typescript sdk";
  };
}
