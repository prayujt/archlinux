{
    description = "Global Environment";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    };

    outputs = { self, nixpkgs }: {
        packages.x86_64-linux.default =
            let pkgs = nixpkgs.legacyPackages.x86_64-linux;
            in pkgs.buildEnv {
                name = "python-env";
                paths = [
                    pkgs.python312
                    pkgs.python312Packages.pip
                    # Add more Python packages here if needed:
                    # pkgs.python312Packages.requests
                    # pkgs.python312Packages.numpy
                ];
            }
    };
}
