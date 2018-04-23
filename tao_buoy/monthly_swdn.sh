#!/bin/bash

buoy=${1}
loc=${2}
name_in=rad
name_out=swdn
base_dir=/work116/htsujino/BUOY
out_dir=swdn_monthly

exe=nc2grads_tao

./${exe}<<EOF
 &nml_buoyrad_mon
   base_dir='${base_dir}',
   out_dir='${out_dir}',
   buoy='${buoy}'
   var_nam='RD_495' ,
   qvar_nam='QSW_5495',
   location='${loc}',
   element_in='${name_in}',
   element_out='${name_out}',
   interval='_dy'
  /
EOF

sed 's/\ //g' tmp.txt > swdn_${loc}.sh

chmod 755 ./swdn_${loc}.sh
. ./swdn_${loc}.sh

echo "${lon} ${lat}"
echo "${numdat} ${stmon} ${styear}"

sed -e s%@base_dir@%${base_dir}% \
    -e s%@out_dir@%${out_dir}% \
    -e s%@loc@%${loc}% \
    -e s%@lon@%${lon}% \
    -e s%@lat@%${lat}% \
    -e s%@num_dat@%${numdat}% \
    -e s%@stmon@%${stmon}% \
    -e s%@styear@%${styear}% \
    swdn_loc.ctl_template > swdn_${loc}.ctl

exit 0
