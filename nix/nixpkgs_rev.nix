import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2021-05-04";
  # url = https://github.com/nixos/nixpkgs-channels/;
  url = https://github.com/PhDyellow/nixpkgs/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/master_fix_pkgs"; #using bleeding edge packages
  rev = "8ce2a15dba3be64eb71004edc79a777100c21325";
})
    { #the attributes to import
      overlays = [
        (import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
        (import /vmshare/cust-nix/Rshell/packages/gurobi_overlay.nix)
        #(import /vmshare/cust-nix/singularity/singularity_overlay.nix)
      ];
    }
