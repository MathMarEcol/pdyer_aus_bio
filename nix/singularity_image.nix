#build this image with
# nix build -f singularity_image.nix -o ~/r-singularity-aus-bio.img  #runs on "singularity_image.nix" in current folder
# If you want to programmatically access the nix store filename:
# sif_image=$(readlink ~/docker_result | sed -e "s/\/nix\/store\///" -e "s/.tar.gz//" )_singularity_conversion #strip leading /nix/store and trailing .tar.gz
#
{ pkgs
  ? import ./nixpkgs_rev.nix
}:

let
  name = "r-singularity-aus-bio";
  allpackages = (import ./all_packages.nix {pkgs = pkgs;});


in
with pkgs; singularity-tools.buildImage { 
  name = name;
  contents = allpackages;
  diskSize = 14192;
  runAsRoot = ''
  mkdir -p /etc
touch /etc/passwd
echo "root:x:0:0:System administrator:/root:/bin/sh" > /etc/passwd
touch /etc/group
echo "root:x:0:" > /etc/group
mkdir -p /.singularity.d
mkdir -p /.singularity.d/env
echo "export LC_ALL=C" >> /.singularity.d/env/91-environment.sh
chmod ugo+x /.singularity.d/env/91-environment.sh
touch /.singularity.d/env/94-appbase.sh
chmod ugo+x /.singularity.d/env/94-appbase.sh

mkdir -p /opt
# mkdir -p /etc/localtime #this is actually a symlink to another directory. don't hardcode it
# mkdir -p /etc/hosts #already done by singularity in version 3.5+
mkdir -p /scratch
mkdir -p /QRISdata
mkdir -p /sw
mkdir -p /sw7
mkdir -p /groups

mkdir -p /bin
ln -s ${runtimeShell} /bin/bash
'';
  }
