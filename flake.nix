{
  description = "nix flake for pocket size fund development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true; # needed for nvidia drivers
        };
        pythonVersion = "3.10.3";
        pythonPackages = ps:
          with ps; [
          ];
        commonPackages = with pkgs; [
          git
          (python3.withPackages pythonPackages)
          python311Packages.greenlet
          python311Packages.numpy
          python311Packages.pandas
          python311Packages.pip
          python311Packages.setuptools
          python311Packages.ray
          uv
          starship
          redis
          gum
          #libgcc
          #libz
          #binutils
          #coreutils
          #expat
        ];

        darwinPackages = with pkgs; [];
        linuxPackages = with pkgs; [];

        cudaPackages = with pkgs; [
          cudatoolkit
          libGLU
          libGL
          xorg.libXi
          xorg.libXmu
          freeglut
          xorg.libXext
          xorg.libX11
          xorg.libXv
          xorg.libXrandr
          zlib
        ];


        commonShellHook = ''
          eval "$(starship init zsh)"
          alias pip="uv pip"
        '';


        cudaShellHook = ''
            export CUDA_PATH=${pkgs.cudatoolkit}
            export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
            export EXTRA_CCFLAGS="-I/usr/include"
            export LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib:${pkgs.cudatoolkit}/lib:${pkgs.cudatoolkit.lib}/lib:$LD_LIBRARY_PATH
            alias pip="PIP_PREFIX='$(pwd)/_build/pip_packages' TMPDIR='$HOME' \pip"
            export PYTHONPATH="$(pwd)/_build/pip_packages/lib/python3.7/site-packages:$PYTHONPATH"
            export PATH="$(pwd)/_build/pip_packages/bin:$PATH"
            unset SOURCE_DATE_EPOCH
        '';

        pipelinesShellHook = ''
            cd pipelines
            uv venv
            source .venv/bin/activate
            uv pip install -r requirements.txt
            ray start --head --port=0

        '';

        pipelineExitHook = ''
          ${pip-freeze}/bin/pip-freeze
        '';

        pip-freeze = pkgs.writeShellScriptBin "pip-freeze" ''
          uv pip freeze | uv pip compile - -o requirements.txt
        '';

        scripts = [
          pip-freeze
        ];

        mkDevShell = pkgs.mkShell {
          nativeBuildInputs =
            commonPackages
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin darwinPackages
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux linuxPackages;

          buildInputs = scripts;
          shellHook = commonShellHook + pipelinesShellHook;
          exitHook = pipelineExitHook;

        };
        mkDevShellCuda = pkgs.mkShell {
          nativeBuildInputs = 
            commonPackages
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin darwinPackages
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux linuxPackages
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux cudaPackages;

          buildInputs = scripts;
          shellHook = commonShellHook + cudaShellHook + pipelinesShellHook;
          exitHook = pipelineExitHook;
        };
      in {
        devShells.default = mkDevShell;
        devShells.pipelines = {
          default = mkDevShell;
          cuda = mkDevShellCuda;
        };
      }
    );
}
