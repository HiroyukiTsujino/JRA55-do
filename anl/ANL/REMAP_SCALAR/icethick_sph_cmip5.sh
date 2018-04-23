#! /bin/sh
#

yearst=${1}
yeared=${2}

################

ftable="/workd/htsujino/CGCM3/scup_tables/table_o2a/file_rmp_ot2a.d"
farea="../spherical/rmp_hice_area_cmip5.gd"

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  flin="../grdmon/hs_ice.${year}01"
  flout="../spherical/hs_hice_TL159_cmip5.${year}01"
  ./remap_scalar.sh ${flin} ${flout} t 1 1 1 -9.99d33 .true. ${ftable} ${farea}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
