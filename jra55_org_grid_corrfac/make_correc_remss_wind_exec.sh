#!/bin/bash -f

set -e

rm -f namelist.windcorrec_mag_ann

ln -sf namelist.windcorrec_mag_ann_remss namelist.windcorrec_mag_ann

./mk_correc_wmag_ann
