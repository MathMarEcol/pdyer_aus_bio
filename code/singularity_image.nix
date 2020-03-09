#build this image with
# nix build -f singularity_image.nix -o ~/docker_result  #runs on "singularity_image.nix" in current folder
# sif_image=$(readlink ~/docker_result | sed -e "s/\/nix\/store\///" -e "s/.tar.gz//" )_singularity_conversion #strip leading /nix/store and trailing .tar.gz
# echo $sif_image
# sudo singularity build ${sif_image}.sif  docker-archive:$(readlink ~/docker_result) #build a sif file and store in current folder
#
{ pkgs
  ? import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-01-20";
  url = https://github.com/nixos/nixpkgs-channels/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/nixos-unstable"; #using bleeding edge packages
  rev = (import ./nixpkgs_rev.nix);
	}) { #the attributes to import
	overlays = [ 
		(import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
	];
  }
}:

let
  name = "r-singularity-aus_bio";
  allpackages = (import ./all_packages.nix {pkgs = pkgs;});


in
with pkgs; dockerTools.buildImage { 
  #breaks because buildImage evals extraCommands BEFORE mkdir $out
  #in addition, the builder is isolated and I won't be able to look up the result
  #make singularity

  extraCommands = '' 
###stuff to make layer singularity compatible here

###
'';
  config = {
    Cmd = [];
    Entrypoint = [
      (writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
  #    set -e
    #!${pkgs.stdenv.shell}
  #    exec /bin/bash
  ''
      )
    ];
  };
  contents = allpackages;
  name = name;
  }
