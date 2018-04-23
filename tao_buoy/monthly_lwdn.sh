#!/bin/bash

buoy=${1}
loc=${2}
name_in=lw
name_out=lwdn
base_dir=/work116/htsujino/BUOY
out_dir=lwdn_monthly

exe=nc2grads_tao

./${exe}<<EOF
 &nml_buoyrad_mon
   base_dir='${base_dir}',
   out_dir='${out_dir}',
   buoy='${buoy}',
   var_nam='Ql_136' ,
   qvar_nam='QLW_5136',
   location='${loc}',
   element_in='${name_in}',
   element_out='${name_out}',
   interval='_dy'
  /
EOF

sed 's/\ //g' tmp.txt > lwdn_${loc}.sh

chmod 755 ./lwdn_${loc}.sh
. ./lwdn_${loc}.sh

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
    lwdn_loc.ctl_template > lwdn_${loc}.ctl

exit 0
