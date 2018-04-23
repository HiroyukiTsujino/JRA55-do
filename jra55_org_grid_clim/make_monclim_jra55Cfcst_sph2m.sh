#!/bin/bash -f

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year"
  exit
fi

# raw
#dir_in=../linkdir/forcing/jra55Cfcst_monthly_TL319r
#dir_out_org=../linkdir/forcing/jra55Cfcst_monclim_TL319r
#dir_out_latlon=../linkdir/forcing/jra55Cfcst_monclim_TL319

# adjusted
dir_in=../linkdir/forcing/jra55Cfcst_v7a_monthly_TL319r
dir_out_org=../linkdir/forcing/jra55Cfcst_v7a_monclim_TL319r
dir_out_latlon=../linkdir/forcing/jra55Cfcst_v7a_monclim_TL319

rm -f namelist.make_monclim

yearst=${1}
yeared=${2}

out_latlon=.true.

#

frac_valid=1.0d0

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@dir_in@%${dir_in}% \
    -e s%@dir_out_org@%${dir_out_org}% \
    -e s%@dir_out_latlon@%${dir_out_latlon}% \
    -e s%@out_latlon@%${out_latlon}% \
    -e s%@frac_valid@%${frac_valid}% \
    namelist.make_monclim_jra55Cfcst_sph2m_template > namelist.make_monclim

./make_monclim
