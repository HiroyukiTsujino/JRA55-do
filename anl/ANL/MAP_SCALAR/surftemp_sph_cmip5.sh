#! /bin/sh
#
# echo 'Usage: grd2nc_ht.sh flin flout'

yearst=${1}
yeared=${2}

################

ftable="/workd/htsujino/CGCM3/scup_tables/table_a2o/file_rmp_a2ot.d"
farea="/workd/htsujino/CGCM3/run-C3_historical_01/ogcm_grid/rmp_temp_area_cmip5.gd"

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  flin="/workd/htsujino/CGCM3/run-C3_historical_01/d_monit_a/m2.${year}01"
  flout="/workd/htsujino/CGCM3/run-C3_historical_01/ogcm_grid/surft_cmip5.${year}01"
  ./map_scalar.sh ${flin} ${flout} t 5 1 1 -9.99d33 .true. ${ftable} ${farea}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
