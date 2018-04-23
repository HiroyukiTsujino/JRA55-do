#!/bin/bash -f

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

#orgdir=/mri-data/jra-55/Hist/Daily/fcst_phy2m
orgdir=../linkdir/forcing/fcst_phy2m
newdir=../linkdir/forcing/jra_org

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  if [ x${day_st} == x ]; then
    i=1
  else
    i=${day_st}
  fi
  if [ x${day_ed} == x ]; then
    iend=`expr 365 + ${leap}`
  else
    iend=${day_ed}
  fi

  echo "iend = ${iend}"

  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}

    if [ ! -e ${newdir}/${yyyymm} ]; then
      echo "creating ${yyyymm}"
      mkdir ${newdir}/${yyyymm}
    fi

    date=${yr0}${mn0}${dy0} 

    for hh in 00 03 06 09 12 15 18 21
    do 
      echo ${i} ${date}${hh}
      #wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${date}${hh} | egrep '(:NCPCP:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${date}${hh} -o ${newdir}/${yyyymm}/lrain.${date}${hh}
      #wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${date}${hh} | egrep '(:ACPCP:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${date}${hh} -o ${newdir}/${yyyymm}/crain.${date}${hh}
      wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${date}${hh} | egrep '(:SRWEQ:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${date}${hh} -o ${newdir}/${yyyymm}/snow.${date}${hh}
    done

    i=$(( $i + 1 ))

  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
