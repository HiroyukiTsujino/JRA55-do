#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
item_erai=${3}
item=${4}

mon_st=1
mon_ed=12

erai_dir='../linkdir/ERA-interim/grads_monthly'
erai_jra='../linkdir/forcing/erai_monthly_TL319r'
erai_jra_latlon='../linkdir/forcing/erai_monthly_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'
erai_cnst='../linkdir/ERA-interim/data'

out_latlon=.true.
undef_out=9.999e20

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
        -e s%@erai_cnst@%${erai_cnst}% \
        -e s%@erai_jra@%${erai_jra}% \
        -e s%@erai_jra_latlon@%${erai_jra_latlon}% \
        -e s%@erai_dir@%${erai_dir}% \
        -e s%@item@%${item}% \
        -e s%@item_erai@%${item_erai}% \
        -e s%@irec@%1% \
        -e s%@out_latlon@%${out_latlon}% \
	-e s%@undef_out@%${undef_out}% \
        namelist.erai_on_jra55_template > namelist.erai_on_jra55
    ./erai_on_jra55_grid
    m=$(( $m + 1 ))
  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
