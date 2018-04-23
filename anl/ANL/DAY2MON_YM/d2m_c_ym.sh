#! /bin/sh -f

iyear=$1
imonth=$2

################

fpathin="../d_monit_o/daily/tos"
fpathout="../cmip5_oc_output/Omon/Table2.2/tos"
tuxy="t"
k_max=1
./day2mon_ym.sh $iyear $imonth "$fpathin" "$fpathout" "$tuxy" $k_max 0.e0

fpathin="../d_monit_o/daily/tossq"
fpathout="../cmip5_oc_output/Omon/Table2.2/tossq"
tuxy="t"
k_max=1
./day2mon_ym.sh $iyear $imonth "$fpathin" "$fpathout" "$tuxy" $k_max 0.e0

fpathin="../d_monit_o/daily/omldamax"
fpathout="../cmip5_oc_output/Omon/Table2.2/omldamax"
tuxy="t"
k_max=1
./day2mon_ym.sh $iyear $imonth "$fpathin" "$fpathout" "$tuxy" $k_max 0.e0

fpathin="../d_monit_o/daily/omldamax"
fpathout="../cmip5_oc_output/Omon/Table2.2/omlmax"
tuxy="t"
k_max=1
./day2monmax_ym.sh $iyear $imonth "$fpathin" "$fpathout" "$tuxy" $k_max 0.e0

 
