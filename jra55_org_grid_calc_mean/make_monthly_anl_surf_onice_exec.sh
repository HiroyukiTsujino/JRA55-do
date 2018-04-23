#!/bin/bash -f

set -e

if [ x${4} = x ]; then
  echo "Usage: ${0} start_year end_year var_name_in var_name_output"
  exit
fi

yearst=${1}
yeared=${2}
var_in=${3}
var_out=${4}

out_latlon=.true.
frac_ratio=1.0d0

org_dir='../linkdir/forcing/jra55anl_6hr_TL319r'
ice_dir='../linkdir/forcing/jra55fcst_3hr_TL319r'
monthly_org_dir='../linkdir/forcing/jra55anl_monthly_TL319r'
monthly_latlon_dir='../linkdir/forcing/jra55anl_monthly_TL319'

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
        -e s%@ice_dir@%${ice_dir}% \
        -e s%@var_in@%${var_in}% \
        -e s%@monthly_org_dir@%${monthly_org_dir}% \
        -e s%@var_out@%${var_out}% \
        -e s%@monthly_latlon_dir@%${monthly_latlon_dir}% \
        -e s%@out_latlon@%${out_latlon}% \
        -e s%@frac_ratio@%${frac_ratio}% \
	namelist.make_monthly_anl_surf_onice_template > namelist.make_monthly_surf_onice

    ./make_org_to_monthly_surf_onice

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
