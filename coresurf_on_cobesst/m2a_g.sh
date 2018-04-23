#!/bin/sh -f

set -e

styear=1948
endyear=2009

base_dir=../linkdir/CORE

#mondir="${base_dir}/core_cobesst_monthly_dec2015"
#anndir="${base_dir}/core_cobesst_annual_dec2015"

#mondir="${base_dir}/core_cobesst_monthly_jan2017"
#anndir="${base_dir}/core_cobesst_annual_jan2017"

mondir="${base_dir}/core_cobesst_monthly_aug2017"
anndir="${base_dir}/core_cobesst_annual_aug2017"

#mondir="${base_dir}/core_monthly_cobe_ly2009"
#anndir="${base_dir}/core_annual_cobe_ly2009"

################

#fpathin="${mondir}/wind10m"
#fpathout="${anndir}/wind10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/sph10m"
fpathout="${anndir}/sph10m"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/tmp10m"
fpathout="${anndir}/tmp10m"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/dtu10m"
fpathout="${anndir}/dtu10m"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/dqu10m"
fpathout="${anndir}/dqu10m"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/wvn10m"
fpathout="${anndir}/wvn10m"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
