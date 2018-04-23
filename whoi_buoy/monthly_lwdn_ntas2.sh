#!/bin/bash

buoy=NTAS
loc=NTAS
name_in=LW
name_out=lwdn
base_dir=/work116/htsujino/BUOY
out_dir=lwdn_monthly

exe=nc2grads_whoi

./${exe}<<EOF
 &nml_buoyrad_mon
  base_dir='${base_dir}',
  out_dir='${out_dir}',
  buoy='${buoy}',
  file_in='OS_NTAS_2012-2013_D_MLTS-1hr.nc'
  var_nam_in='${name_in}',
  element_out='${name_out}',
 /
EOF

sed 's/\ //g' tmp.txt > lwdn_${loc}.sh

chmod 755 ./lwdn_${loc}.sh
. ./lwdn_${loc}.sh

echo "${lon} ${lat}"
echo "${numdat} ${stmon} ${styear}"

sed -e s%@base_dir@%${base_dir}% \
    -e s%@out_dir@%${out_dir}% \
    -e s%@loc@%${location}% \
    -e s%@lon@%${lon}% \
    -e s%@lat@%${lat}% \
    -e s%@num_dat@%${numdat}% \
    -e s%@stmon@%${stmon}% \
    -e s%@styear@%${styear}% \
    lwdn_loc.ctl_template > lwdn_${location}_2.ctl

exit 0
