#!/bin/bash -f

set -e

dir_in=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/Bamber_2012_Greenland_025x025/clim
dir_out=/work116/htsujino/SURF_FLUX/forcing/Bamber_2012_Greenland_025x025/clim

# normal year

file_river_in=${dir_in}/fwf_green_025x025_365dy
file_mask_out=${dir_out}/mask_greenland_bamber_2012.gd
ibyr=1961
ieyr=1961
l_clim=.true.
days=365
undef_in=0.0

sed -e s%@file_river_base@%${file_river_in}% \
    -e s%@file_mask_out@%${file_mask_out}% \
    -e s%@num_data@%${days}% \
    -e s%@l_clim@%${l_clim}% \
    -e s%@undef_in@%${undef_in}% \
    -e s%@ibyr@%${ibyr}% \
    -e s%@ieyr@%${ieyr}% \
    namelist.scan_noyrev_template > namelist.scan_noyrev

./scan_noyrev
