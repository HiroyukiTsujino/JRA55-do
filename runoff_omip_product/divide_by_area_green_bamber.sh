#!/bin/bash -f

set -e

l_area_out=.false.
file_area_out=dummy.gd

dir_in=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/Bamber_2012_Greenland_025x025/clim
dir_out=/work116/htsujino/SURF_FLUX/forcing/jra55-do-v1_1-river

# normal year

for days in 365 366
do
  file_river_in=${dir_in}/fwf_green_025x025_${days}dy
  file_river_out=${dir_out}/runoff_Greenland_Bamber.${days}dy

  sed -e s%@file_river_in@%${file_river_in}% \
      -e s%@file_river_out@%${file_river_out}% \
      -e s%@num_data@%${days}% \
      -e s%@l_area_out@%${l_area_out}% \
      -e s%@file_area_out@%${file_area_out}% \
  namelist.divide_by_cellarea_template > namelist.divide_by_cellarea

  ./divide_by_cellarea

done
