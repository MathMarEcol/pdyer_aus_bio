#call this shell with
#/vmshare/cust-nix/nix_shell_pin.sh r_shell.nix ~/.nixshellgc
{ pkgs
  ? import
    (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-03-09";
  # url = https://github.com/nixos/nixpkgs-channels/;
  url = https://github.com/PhDyellow/nixpkgs/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/f_rzmq"; #using bleeding edge packages
  rev = (import ./nixpkgs_rev.nix);
	})
    { #the attributes to import
	overlays = [ 
		(import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
	];
  }
}:

let
  name = "r-singularity-aus_bio-shell";
  rpackages = (import ./r_packages.nix {pkgs = pkgs;}); #already in all_packages, but used here for RStudio
  allpackages = (import ./all_packages.nix {pkgs = pkgs;});
in 
pkgs.stdenv.mkDerivation {
  name = name;
  version = "1";

  nativeBuildInputs = with pkgs; [
  ];
  buildInputs = allpackages ++ [
    (pkgs.rstudioWrapper.override {
      packages = [
      ] ++ rpackages;
    })
  ];
  }

