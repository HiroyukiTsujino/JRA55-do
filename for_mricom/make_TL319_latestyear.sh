#!/bin/bash

set -e

if [ x${4} == x ]; then
   echo "Usage: ${0} item start_year end_year end_day"
   exit
fi

item=${1}
yearst=${2}
yeared=${3}
dayed=${4}

################

# V01
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_latlon_v1_1
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v01/TL319_grid

# V02
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_latlon    # slprs, brtmp, ice
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_latlon_c1 # u10m, v10m, tmp10m
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_latlon_c2 # dswrf, dlwrf, prcp, sph10m
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_latlon_c3 # tmp10m u10m v10m
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra_latlon_c4 # sph10m

#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_org_for_mricom/TL319_grid
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_c3c4_for_mricom/TL319_grid

# V03
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_latlon_e2 # tmp10m
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_latlon_e3 # slprs, sph10m, u10m, v10m
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_latlon_e4 # dswrf, dlwrf, prcp, rain, snow

#newdir=/work115/htsujino/SURF_FLUX/forcing/jra_for_mricom_v03/TL319_grid

# V04
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_latlon_e5 # tmp10m sph10m
#newdir=/work115/htsujino/SURF_FLUX/forcing/jra_for_mricom_v04/TL319_grid

# V05
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_latlon_e7 # tmp10m
#newdir=/work115/htsujino/SURF_FLUX/forcing/jra_for_mricom_v05/TL319_grid

# V07
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra55fcst_3hr_TL319          # slprs brtmp ice
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v7_prod4_3hr_TL319 # tmp10m sph10m
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v7_prod1_3hr_TL319 # u10m v10m
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v7_rad2_3hr_TL319  # dswrf dlwrf
#####orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v7_prcp1_3hr_TL319 # prcp snow rain
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v07/TL319_grid
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_support/TL319_grid # brtmp ice

# V1.0
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v1_0_prcp_3hr_TL319 # prcp rain snow
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v1_0/TL319_grid

# V1.1
#orgdir=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v1_1_prcp_3hr_TL319 # prcp rain snow

#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v1_1/TL319_grid
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_support/TL319_grid # brtmp ice

# V1.2
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra55fcst_3hr_TL319          # slprs brtmp ice
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_2_prod4_3hr_TL319 # tmp10m sph10m
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_2_prod1_3hr_TL319 # u10m v10m
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_2_rad2_3hr_TL319  # dswrf dlwrf
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_2_prcp2_3hr_TL319 # prcp snow

# V1.3
# ocsv011
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra55fcst_3hr_TL319          # slprs brtmp ice
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_prod4_3hr_TL319 # tmp10m sph10m
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_prod1_3hr_TL319 # u10m v10m
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_rad2_3hr_TL319  # dswrf dlwrf
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_prcp2_3hr_TL319 # prcp snow

#orgdir=../linkdir/work/jra55fcst_3hr_TL319            # slprs brtmp ice
#orgdir=../linkdir/work/jra55fcst_v1_3_prod4_3hr_TL319 # tmp10m sph10m
#orgdir=../linkdir/work/jra55fcst_v1_3_prod1_3hr_TL319 # u10m v10m
#orgdir=../linkdir/work/jra55fcst_v1_3_rad2_3hr_TL319  # dswrf dlwrf
#orgdir=../linkdir/work/jra55fcst_v1_3_prcp2_3hr_TL319 # prcp snow

# V1.4
#orgdir=/work116/htsujino/SURF_FLUX/forcing/jra55fcst_3hr_TL319          # slprs brtmp ice
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prod4_3hr_TL319 # tmp10m sph10m
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prod1_3hr_TL319 # u10m v10m
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_rad2_3hr_TL319  # dswrf dlwrf
#orgdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_prcp2_3hr_TL319 # prcp snow

# NEWDIR

# ocsv011
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_slprs/TL319_grid   # slprs
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_support/TL319_grid # brtmp ice
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v1_2/TL319_grid
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v1_3/TL319_grid
#newdir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom_v1_4/TL319_grid

# CX2550
newdir=../linkdir/products/version_1_5/grads # atmospheric variables
#newdir=../linkdir/products/support/grads # brtmp ice

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  iend=${dayed}
  echo "iend = ${iend}"
  outfile=${item}.${year}

  if [ ! -e ${newdir}/${year} ]; then
    echo "creating ${year}"
    mkdir -p ${newdir}/${year}
  fi

  if [ -e ${newdir}/${year}/${outfile} ]; then
    echo "remove older file"
    rm -f ${newdir}/${year}/${outfile}
  fi

  touch ${newdir}/${year}/${outfile}
  echo "${newdir}/${year}/${outfile} created" 
  i=1
  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}

    date=${yr0}${mn0}${dy0} 

    for hh in 00 03 06 09 12 15 18 21
    do 
      echo ${i} ${date}${hh}
      cat ${orgdir}/${yyyymm}/${item}.${date}${hh} >> ${newdir}/${year}/${outfile}
    done

    i=$(( $i + 1 ))

  done

  cp ${orgdir}/${year}01/${item}.${year}010100 ${newdir}/${year}/${item}.${year}010100
  cp ${orgdir}/${year}01/${item}.${year}010103 ${newdir}/${year}/${item}.${year}010103

  yearn=`expr ${year} + 1`
  year=${yearn}

done
