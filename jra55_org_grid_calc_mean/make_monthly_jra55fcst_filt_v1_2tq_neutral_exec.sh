#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

yearst=${1}
yeared=${2}

out_latlon=.true.

org_dir='../linkdir/forcing/jra55fcst_filt_v1_2tq_3hr_TL319r'
monthly_org_dir='../linkdir/forcing/jra55fcst_filt_v1_2tq_monthly_TL319r'
monthly_latlon_dir='../linkdir/forcing/jra55fcst_filt_v1_2tq_monthly_TL319'

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  for m in `seq -w 1 12`
  do
    sed -e s%@year@%${year}% \
        -e s%@month@%${m}% \
        -e s%@org_dir@%${org_dir}% \
        -e s%@monthly_org_dir@%${monthly_org_dir}% \
        -e s%@monthly_latlon_dir@%${monthly_latlon_dir}% \
        -e s%@out_latlon@%${out_latlon}% \
	namelist.make_monthly_fcst_neutral_template > namelist.make_monthly_wind

    ./make_org_to_monthly_wind

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
