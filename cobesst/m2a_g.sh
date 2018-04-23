#!/bin/sh -f

if [ x${2} = x ]; then
  echo 'Usage: m2a_sh start_year end_year'
fi

styear=${1}
endyear=${2}

mondir="../linkdir/COBESST/monthly/yyyymm"
anndir="../linkdir/COBESST/annual/grads"


################

fpathin="${mondir}/sst-glb"
fpathout="${anndir}/sst-glb"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/ice-glb"
fpathout="${anndir}/ice-glb"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
