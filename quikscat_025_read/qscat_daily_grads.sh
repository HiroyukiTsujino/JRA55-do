#! /bin/sh
#

if [ x${2} == "x" ]; then 
  echo 'Usage: qscat_daily_grads.sh start_year end_year'
  exit
fi

yearst=${1}
yeared=${2}

homedir=`eval pwd`

################

undef=-9.99e33

orgdir="/worke/htsujino/QuikSCAT/L3/jpl/v2/hdf"
outdir="/worke/htsujino/QuikSCAT/L3/jpl/v2/grads_daily"

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  iend=`expr 365 + ${leap}`
  i=1
  #i=200 # 1999 start
  #iend=325 # 2009 end
  while [ ${i} -le ${iend} ];
  do
    day=$( printf %03d ${i} )
    cd ${orgdir}/${year}
    fn=`eval ls QS_XWGRD3_${year}${day}.???????????`
    if [ x${fn} = x ]; then
      level3_file='missing_file'
    else
      level3_file=${orgdir}/${year}/${fn}
    fi
    cd ${homedir}
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}
    date=${yr0}${mn0}${dy0} 

    wind_grads=${outdir}/${year}/qscat_wind.${date}

    echo "input  = ${level3_file}"
    echo "output = ${wind_grads}"

    sed -e "
    s@%level3_file%@${level3_file}@g
    s@%wind_grads%@${wind_grads}@g
    s@%undef%@${undef}@g
    " < namelist.qscat_daily_template > namelist.qscat_daily

    ./qscat2daily

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}
done
