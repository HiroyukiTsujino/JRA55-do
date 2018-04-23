#!/bin/sh

yyyymm=${1}

flin1="/worke/htsujino/QuikSCAT/L3/jpl/v2/grads_monthly/qscat_wind_mask.${yyyymm}"
flot1="/worke/htsujino/QuikSCAT/L3/jpl/v2/grads_monthly_on_jra55/jra55_wind_mask.${yyyymm}"
file_topo="/worke/htsujino/QuikSCAT/L3/jpl/v2/grads_monthly_on_jra55/jra55_qscat_topo.d"
file_mask="/worke/htsujino/QuikSCAT/L3/jpl/v2/grads_monthly_on_jra55/jra55_qscat_mask.gd"
l_mask_out=.false.
num_data=3

./qscat_to_jra<<EOF
&nml_hintpol
 flin1="${flin1}",
 flot1="${flot1}",
 num_data=${num_data}
 undef_out=-9.99e33
 file_topo="${file_topo}"
 file_mask="${file_mask}"
 l_mask_out=${l_mask_out}
 /
EOF
