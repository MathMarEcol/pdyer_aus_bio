#!/bin/sh

rsync -irc  /QRISdata/Q1215/bioORACLE /90days/uqpdyer/rdm_mirror/Q1215
rsync -irc  /QRISdata/Q1215/AusCPR /90days/uqpdyer/rdm_mirror/Q1215
rsync -irc  /QRISdata/Q1215/ShapeFiles/World_EEZ_v8 /90days/uqpdyer/rdm_mirror/Q1215/ShapeFiles

rsync -irc  /QRISdata/Q1216/pdyer/pdyer_aus_bio /90days/uqpdyer/rdm_mirror/Q1216/pdyer/
