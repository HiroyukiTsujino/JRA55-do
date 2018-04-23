#! /bin/sh
#
# echo 'Usage: grd2nc_ht.sh flin flout'

yearst=${1}
yeared=${2}

################

ftable="/workd/htsujino/CGCM3/scup_tables/table_o2a/file_rmp_ou2a.d"
farea="../spherical/rmp_icevec_area_cmip5_scupbug.gd"

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  flin="../grdmon/hs_iceuv.${year}01"
  floutu="../spherical/hs_iceu_TL159_cmip5_revised.${year}01"
  floutv="../spherical/hs_icev_TL159_cmip5_revised.${year}01"
  ./remap_vector.sh ${flin} ${flin} ${floutu} ${floutv} 1 2 1 1 -9.99d33 .true. ${ftable} ${farea}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
