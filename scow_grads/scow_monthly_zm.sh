#! /bin/sh
#

set -e

datdir="/work116/htsujino/SCOW/grads"

homedir=`eval pwd`

################

undef=-9.99e33

year=${yearst}

mn=1
mend=12

while [ ${mn} -le ${mend} ];
do
  mn0=$( printf %02d $mn )
  wind_grads=${datdir}/wind_spd_monthly.m${mn0}
  wind_zm_grads=${datdir}/wind_spd_monthly_zm.m${mn0}

  sed -e "
  s@%wind_grads%@${wind_grads}@g
  s@%wind_zm_grads%@${wind_zm_grads}@g
  " < namelist.zonalmean_template > namelist.zonalmean

  ./zonal_mean

  mn=$(( ${mn} + 1 ))
done
