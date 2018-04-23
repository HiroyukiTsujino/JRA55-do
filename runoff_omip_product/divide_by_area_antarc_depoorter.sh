#!/bin/bash -f

set -e

l_area_out=.false.
file_area_out=dummy.gd

dir_in=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/Depoorter_2013_Antarctica/clim025x025
dir_out=/work116/htsujino/SURF_FLUX/forcing/jra55-do-v1_1-river

file_river_in=${dir_in}/BMF_025x025.dat
file_river_out=${dir_out}/runoff_Antarctica_Depoorter_BMF.gd
num_data=1

sed -e s%@file_river_in@%${file_river_in}% \
    -e s%@file_river_out@%${file_river_out}% \
    -e s%@num_data@%${num_data}% \
    -e s%@l_area_out@%${l_area_out}% \
    -e s%@file_area_out@%${file_area_out}% \
namelist.divide_by_cellarea_template > namelist.divide_by_cellarea

./divide_by_cellarea

file_river_in=${dir_in}/CF_025x025.dat
file_river_out=${dir_out}/runoff_Antarctica_Depoorter_CF.gd
num_data=1

sed -e s%@file_river_in@%${file_river_in}% \
    -e s%@file_river_out@%${file_river_out}% \
    -e s%@num_data@%${num_data}% \
    -e s%@l_area_out@%${l_area_out}% \
    -e s%@file_area_out@%${file_area_out}% \
namelist.divide_by_cellarea_template > namelist.divide_by_cellarea

./divide_by_cellarea
