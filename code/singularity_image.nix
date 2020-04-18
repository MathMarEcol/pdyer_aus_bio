#build this image with
# nix build -f singularity_image.nix -o ~/docker_result  #runs on "singularity_image.nix" in current folder
# sif_image=$(readlink ~/docker_result | sed -e "s/\/nix\/store\///" -e "s/.tar.gz//" )_singularity_conversion #strip leading /nix/store and trailing .tar.gz
# echo $sif_image
# sudo singularity build ${sif_image}.sif  docker-archive:$(readlink ~/docker_result) #build a sif file and store in current folder
#
{ pkgs
  ? import
    (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-04-17";
  # url = https://github.com/nixos/nixpkgs-channels/;
  url = https://github.com/PhDyellow/nixpkgs/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/f_rzmq_unstable"; #using bleeding edge packages
  rev = (import ./nixpkgs_rev.nix);
	})
    { #the attributes to import
	overlays = [ 
		(import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
	];
  }
}:

let
  name = "r-singularity-aus_bio";
  allpackages = (import ./all_packages.nix {pkgs = pkgs;});


in
with pkgs; singularity-tools.buildImage { 
  name = name;
  contents = allpackages;
  diskSize = 1192;
  runAsRoot = ''
  mkdir -p /etc
touch /etc/passwd
touch /etc/group
mkdir -p /.singularity.d
mkdir -p /.singularity.d/env
echo "export LC_ALL=C" >> /.singularity.d/env/91-environment.sh
'';
  }
