#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
day_st=${3}
day_ed=${4}

# 1999 1999 200 365
# 2009 2009   1 323

#s_w=0.7d0 # 1
#s_w=0.5d0 # 2
#s_w=0.3d0 # 3
s_w=0.4d0 # 4

qscat_dir='/work115/htsujino/QuikSCAT/qscat/bmaps_v04/grads_daily_strict'
gcurr_dir='/denkei-shared/og1/htsujino/GlobCurrent/global_025_deg/total_hs/grads_daily'
tq_daily='../linkdir/forcing/jra55fcst_v1_2_tq2m_daily_TL319r'
fcst_daily='../linkdir/forcing/jra_daily_org'
qfree_red='/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_4_daily_TL319r'
qfree_latlon='/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_4_daily_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'
cobe_dir='/work116/htsujino/COBESST/daily/grads'

out_latlon=.true.

gcurr_latlon='/work113/htsujino/SURF_FLUX/forcing/gcurr_on_qscat_daily_TL319'
out_gcurr=.true.

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

  if [ ${year} -eq 1999 ]; then
    i=200
  fi
  if [ ${year} -eq 2009 ]; then
    iend=323
  fi

  echo "year = ${year}  istart = ${i}  iend = ${iend}"

  while [ $i -le $iend ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )
 
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyy=${yr0}
    yyyymm=${yr0}${mn0}
    yyyymmdd=${yr0}${mn0}${dy0}

    if [ ! -e ${qfree_red}/${yyyymm} ]; then
      echo "creating ${qfree_red}/${yyyymm}"
      mkdir ${qfree_red}/${yyyymm}
    fi

    if [ ! -e ${qfree_latlon}/${yyyymm} ]; then
      echo "creating ${qfree_latlon}/${yyyymm}"
      mkdir ${qfree_latlon}/${yyyymm}
    fi

    sed -e s%@yyyy@%${yyyy}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@yyyymmdd@%${yyyymmdd}% \
        -e s%@qscat_dir@%${qscat_dir}% \
        -e s%@gcurr_dir@%${gcurr_dir}% \
        -e s%@tq_daily@%${tq_daily}% \
        -e s%@fcst_daily@%${fcst_daily}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@qfree_red@%${qfree_red}% \
        -e s%@qfree_latlon@%${qfree_latlon}% \
        -e s%@out_latlon@%${out_latlon}% \
        -e s%@gcurr_latlon@%${gcurr_latlon}% \
        -e s%@out_gcurr@%${out_gcurr}% \
        -e s%@s_w@%${s_w}% \
        -e s%@cobe_dir@%${cobe_dir}% \
        -e s%@irec@%${i}% \
	namelist.qscat_free_wind_template > namelist.qscat_free_wind

    ./quikscat_to_current_free

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
