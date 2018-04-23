#! /bin/sh
#
# echo 'Usage: grd2nc_ht.sh flin flout'

yearst=${1}
yeared=${2}

################

ftable="/workd/htsujino/CGCM3/scup_tables/table_a2o/file_rmp_a2ou.d"
farea="/workd/htsujino/CGCM3/run-C3_historical_01/ogcm_grid/rmp_wind_area_cmip5_revised.gd"

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  flin="/workd/htsujino/CGCM3/run-C3_historical_01/d_monit_a/m2.${year}01"
  floutu="/workd/htsujino/CGCM3/run-C3_historical_01/ogcm_grid/surfu_cmip5_revised.${year}01"
  floutv="/workd/htsujino/CGCM3/run-C3_historical_01/ogcm_grid/surfv_cmip5_revised.${year}01"
  ./map_vector.sh ${flin} ${flin} ${floutu} ${floutv} 3 4 1 1 -9.99d33 .true. ${ftable} ${farea}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
