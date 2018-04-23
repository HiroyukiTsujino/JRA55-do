#! /bin/sh

if [ x${3} == x ]; then
   echo "Usage: ./make_force_trp.sh start_year end_year item"
   exit
fi

yearst=${1}
yeared=${2}
item=${3}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  yearb=$(( ${year} - 1 ))
  yearn=$(( ${year} + 1 ))
  echo ${yearb} ${year} ${yearn}

  flout="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/force/${item}.${year}.dat"

  flinb="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${yearb}/${item}.${yearb}123121"
  flinm="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/${item}.${year}"
  flinn0="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${yearn}/${item}.${yearn}010100"
  flinn1="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${yearn}/${item}.${yearn}010103"

  \rm ${flout}
  touch ${flout}
  cat ${flinb} >> ${flout}
  cat ${flinm} >> ${flout}
  cat ${flinn0} >> ${flout}
  cat ${flinn1} >> ${flout}

  year=${yearn}

done
