#!/bin/sh

#All processing is done in 90days
#RDM is for storage.



rsync -irc /90days/uqpdyer/rdm_mirror/Q1215/bioORACLE /QRISdata/Q1215/

tar --xz -cf  /90days/uqpdyer/rdm_mirror/Q1216/pdyer/pdyer_aus_bio.tar.xz /90days/uqpdyer/rdm_mirror/Q1216/pdyer/pdyer_aus_bio
rsync -irc /90days/uqpdyer/rdm_mirror/Q1216/pdyer/pdyer_aus_bio.tar.xz /QRISdata/Q1215/pdyer/pdyer_aus_bio.tar.xz
