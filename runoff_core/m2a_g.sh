#!/bin/sh -f

set -e

styear=1948
endyear=2007

mondir="/work116/htsujino/CORE/core_river_monthly_cobesst"
anndir="/work116/htsujino/CORE/core_river_annual_cobesst"


################

fpathin="${mondir}/runoff"
fpathout="${anndir}/runoff"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20
