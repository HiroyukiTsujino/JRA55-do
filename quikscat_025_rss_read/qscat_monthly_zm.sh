#! /bin/sh
#

if [ x${3} == "x" ]; then 
  echo 'Usage: qscat_monthly_zm.sh start_year end_year criteria[strict/loose]'
  exit
fi

yearst=${1}
yeared=${2}
criteria=${3}

if [ "${criteria}" = "strict" ]; then
  strict=.true.
  datdir="/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_monthly_strict"
else
  strict=.false.
  datdir="/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_monthly"
fi

homedir=`eval pwd`

################

undef=-9.99e33

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  mend=12

  mn=1
  if [ ${year} -eq 1999 ]; then
    mn=8
  fi
  if [ ${year} -eq 2009 ]; then
    mend=10
  fi

  while [ ${mn} -le ${mend} ];
  do
    yr0=$( printf %04d $year )
    mn0=$( printf %02d $mn )

    yyyymm=${yr0}${mn0}
    wind_grads=${datdir}/qscat_wind_rss_v4.${yyyymm}
    wind_zm_grads=${datdir}/qscat_wind_rss_v4_zm.${yyyymm}

    #echo "${wind_grads}"
    #echo "${wind_zm_grads}"

    sed -e "
    s@%wind_grads%@${wind_grads}@g
    s@%wind_zm_grads%@${wind_zm_grads}@g
    " < namelist.zonalmean_template > namelist.zonalmean

    ./zonal_mean

    mn=$(( ${mn} + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}
done
