#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
item_era40=${3}
item=${4}
mon_st=1
mon_ed=12

era40_dir='/work115/htsujino/ERA-40/grads_monthly'
era40_jra='/work115/htsujino/SURF_FLUX/forcing/era40_monthly_TL319r'
era40_jra_latlon='/work115/htsujino/SURF_FLUX/forcing/era40_monthly_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'
era40_cnst='/work115/htsujino/ERA-40/data'

#out_latlon=.true. # always output

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  if [ x${mon_st} == x ]; then
    m=1
  else
    m=${mon_st}
  fi
  if [ x${mon_ed} == x ]; then
    mend=12
  else
    mend=${mon_ed}
  fi

  while [ $m -le $mend ];
  do
    yr0=$( printf %04d $year )
    mn0=$( printf %02d $m )
    yyyymm=${yr0}${mn0}
    sed -e s%@yyyymm@%${yyyymm}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@era40_cnst@%${era40_cnst}% \
        -e s%@era40_jra@%${era40_jra}% \
        -e s%@era40_jra_latlon@%${era40_jra_latlon}% \
        -e s%@era40_dir@%${era40_dir}% \
        -e s%@item@%${item}% \
        -e s%@item_era40@%${item_era40}% \
        -e s%@irec@%1% \
        namelist.era40_on_jra55_template > namelist.era40_on_jra55
    ./era40_on_jra55_grid
    m=$(( $m + 1 ))
  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
