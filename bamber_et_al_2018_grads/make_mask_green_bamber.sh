#!/bin/bash -f

set -e

dir_in=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/Bamber_et_al_2018
dir_out=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/Bamber_et_al_2018

# normal year

file_river_in=${dir_in}/grads_monthly/Bamber_2018.
file_mask_out=${dir_out}/mask/mask_Bamber_2018_Grn.gd
ibyr=1958
ieyr=2016
undef_in=0.0

sed -e s%@file_river_base@%${file_river_in}% \
    -e s%@file_mask_out@%${file_mask_out}% \
    -e s%@undef_in@%${undef_in}% \
    -e s%@ibyr@%${ibyr}% \
    -e s%@ieyr@%${ieyr}% \
    namelist.scan_river_template > namelist.scan_river

./scan_bamber_2018
