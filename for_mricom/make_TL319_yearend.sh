#!/bin/bash -f

if [ x${3} == x ]; then
   echo "Usage: ./make_mricom_TL319.sh item start_year end_year"
   exit
fi

item=${1}
yearst=${2}
yeared=${3}

################

#orgdir=/worke/htsujino/SURF_FLUX/forcing/jra_latlon    # slprs
#orgdir=/worke/htsujino/SURF_FLUX/forcing/jra_latlon_c1 # dswrf, dlwrf, u10m, v10m
#orgdir=/worke/htsujino/SURF_FLUX/forcing/jra_latlon_c3 # tmp2m
orgdir=/worke/htsujino/SURF_FLUX/forcing/jra_latlon_c4 # sph2m, prcp
newdir=/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  \rm ${newdir}/${year}/${item}.${year}123100
  cp ${orgdir}/${year}12/${item}.${year}123121 ${newdir}/${year}/${item}.${year}123121
  yearn=`expr ${year} + 1`
  year=${yearn}

done
