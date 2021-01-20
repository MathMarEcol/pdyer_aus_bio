import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-05-01";
  # url = https://github.com/nixos/nixpkgs-channels/;
  url = https://github.com/PhDyellow/nixpkgs/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/master_plus_patches_3"; #using bleeding edge packages
  rev = "a10b00e080415531e09eb4c9ec6b195f54a50625";





})
    { #the attributes to import
      overlays = [
        (import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
        (import /vmshare/cust-nix/Rshell/packages/gurobi_overlay.nix)
        #(import /vmshare/cust-nix/singularity/singularity_overlay.nix)
      ];
    }
