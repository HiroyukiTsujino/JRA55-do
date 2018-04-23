#! /bin/sh
#

set -e

if [ x${3} == "x" ]; then 
  echo 'Usage: qscat_daily_grads.sh start_year end_year criteria[strict/loose]'
  exit
fi

yearst=${1}
yeared=${2}
criteria=${3}

if [ "${criteria}" = "strict" ]; then
  strict=.true.
  outdir="/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily_strict"
else
  strict=.false.
  outdir="/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily"
fi

homedir=`eval pwd`

################

undef=-9.99e33

orgdir="/work115/htsujino/QuikSCAT/qscat/bmaps_v04"

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  iend=`expr 365 + ${leap}`

  i=1
  if [ ${year} -eq 1999 ]; then
    i=200
  fi
  if [ ${year} -eq 2009 ]; then
    iend=323
  fi

  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    echo "${yr} ${mn} ${dy} ${i}"

    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    cd ${orgdir}/y${yr0}/m${mn0}
    if [ -s qscat_${yr0}${mn0}${dy0}v4 ]; then
      qscat_file=${orgdir}/y${yr0}/m${mn0}/qscat_${yr0}${mn0}${dy0}v4
    else
      qscat_file='missing_file'
    fi
#    fn=`eval ls qscat_${yr0}${mn0}${dy0}v4`
#    if [ x${fn} = x ]; then
#      qscat_file='missing_file'
#    else
#      qscat_file=${orgdir}/y${yr0}/m${mn0}/${fn}
#    fi
    cd ${homedir}
    yyyymm=${yr0}${mn0}
    date=${yr0}${mn0}${dy0} 
    wind_grads=${outdir}/${year}/qscat_wind_rss_v4.${date}

    echo "input  = ${qscat_file}"
    echo "output = ${wind_grads}"

    sed -e "
    s@%qscat_file%@${qscat_file}@g
    s@%wind_grads%@${wind_grads}@g
    s@%undef%@${undef}@g
    s@%strict%@${strict}@g
    " < namelist.qscat_daily_template > namelist.qscat_daily

    ./qscat_rss_to_daily

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}
done
