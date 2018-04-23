#!/bin/bash -f

set -e

if [ x${4} = x ]; then
  echo "Usage: ${0} start_year end_year var_name_input var_name_output"
  exit
fi

yearst=${1}
yeared=${2}
var_in=${3}
var_out=${4}

out_latlon=.false.

frac_ratio=1.0d0

# raw data
#org_dir='../linkdir/forcing/jra55fcst_3hr_TL319r'
#monthly_org_dir='../linkdir/forcing/jra55fcst_monthly_TL319r'
#monthly_latlon_dir='../linkdir/forcing/jra55fcst_monthly_TL319'

# adjusted
org_dir='../linkdir/forcing/jra55fcst_v7_3hr_tmp2m_TL319r'
monthly_org_dir='../linkdir/forcing/jra55fcst_v7a_monthly_TL319r'
monthly_latlon_dir='../linkdir/forcing/jra55fcst_v7a_monthly_TL319'

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
        -e s%@var_in@%${var_in}% \
        -e s%@monthly_org_dir@%${monthly_org_dir}% \
        -e s%@var_out@%${var_out}% \
        -e s%@monthly_latlon_dir@%${monthly_latlon_dir}% \
        -e s%@out_latlon@%${out_latlon}% \
        -e s%@frac_ratio@%${frac_ratio}% \
        namelist.make_monthly_fcst_surf_template > namelist.make_monthly_surf

    ./make_org_to_monthly_surf

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
