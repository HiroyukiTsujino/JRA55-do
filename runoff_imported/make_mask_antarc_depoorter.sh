#!/bin/bash -f

set -e

dir_in=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/Depoorter_2013_Antarctica/clim025x025
dir_out=/work116/htsujino/SURF_FLUX/forcing/Depoorter_2013_Antarctica_025x025

# normal year

file_river_in=${dir_in}/CF_025x025.dat
file_mask_out=${dir_out}/mask_antarctica_depoorter_2013.cf.gd
ibyr=1961
ieyr=1961
l_clim=.true.
days=1
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

#

file_river_in=${dir_in}/BMF_025x025.dat
file_mask_out=${dir_out}/mask_antarctica_depoorter_2013.bmf.gd
ibyr=1961
ieyr=1961
l_clim=.true.
days=1
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
