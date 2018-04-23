#!/bin/sh -f

set -e

styear=1948
endyear=2009

base_dir=../linkdir/CORE

mondir="${base_dir}/core_1x1_monthly"
anndir="${base_dir}/core_1x1_annual"


################

fpathin="${mondir}/prcp"
fpathout="${anndir}/prcp"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20
