import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-05-01";
  # url = https://github.com/nixos/nixpkgs-channels/;
  url = https://github.com/PhDyellow/nixpkgs/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/p_update_r_4.0.3"; #using bleeding edge packages
  rev = "4b2dd431f1e4c4652538bdd73deb2cb76d08e6b8";



})
    { #the attributes to import
      overlays = [
        (import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
        (import /vmshare/cust-nix/Rshell/packages/gurobi_overlay.nix)
        #(import /vmshare/cust-nix/singularity/singularity_overlay.nix)
      ];
    }
