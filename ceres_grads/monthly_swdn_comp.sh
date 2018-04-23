#!/bin/bash

loc=${1}
elem_buoy=swdn
elem_ceres=swdn
base_dir=/work116/htsujino/BUOY
out_dir=swdn_monthly

exe=compare_rad_buoy

. ../tao_buoy/swdn_${loc}.sh

sed -e s%@loc@%${loc}% \
    -e s%@lon@%${lon}% \
    -e s%@lat@%${lat}% \
    -e s%@elem_buoy@%${elem_buoy}% \
    -e s%@elem_ceres@%${elem_ceres}% \
    -e s%@outdir@%${out_dir}% \
    namelist.rad_comp_template > namelist.rad_comp

./${exe}

sed -e s%@loc@%${loc}% \
    -e s%@lon@%${lon}% \
    -e s%@lat@%${lat}% \
    swdn_comp.ctl_template > ${out_dir}/swdn_${loc}_comp.ctl

exit 0
