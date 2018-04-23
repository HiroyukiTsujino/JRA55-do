#!/bin/bash -f

yyyymmddhh=${1}
yyyymm=${2}
yyyymmddhhb=${3}
yyyymmb=${4}

yyyymmddhha=${yyyymmddhh}
yyyymma=${yyyymm}

newdir=../linkdir/forcing/jra_latlon

################

echo ${i} ${yyyymmddhh} ${yyyymmddhhb} ${yyyymmddhha}
sed -e s/@yyyymmddhh@/${yyyymmddhh}/ \
    -e s/@yyyymmddhha@/${yyyymmddhha}/ \
    -e s/@yyyymmddhhb@/${yyyymmddhhb}/ \
    -e s/@yyyymm@/${yyyymm}/ \
    -e s/@yyyymma@/${yyyymma}/ \
    -e s/@yyyymmb@/${yyyymmb}/ \
    namelist.shift_height_zrough_template > namelist.shift_height_zrough
./shift_2m_to_10m_zrough
