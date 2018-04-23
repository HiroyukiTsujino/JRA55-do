#!/bin/bash -f

set -e 

if [ x${2} = x ]; then
  echo "Usage: ${0} start_year end_year (start_month end_month)"
  exit
fi

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

iter_ocean=47
iter_all=3

jra_dir=../linkdir/forcing/jra55fcst_v1_4_prod3_monthly_TL319
core_dir=../linkdir/forcing/core_monthly_TL319
had_dir=/work116/htsujino/HadISST/monthly/grads
cobe_dir=/work116/htsujino/COBESST/monthly/yyyymm
corr_dir=../linkdir/forcing/jra55fcst_corrfac_v1_4_prod3_TL319

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  if [ x${mon_st} == x ]; then
    i=1
  else
    i=${mon_st}
  fi
  if [ x${mon_ed} == x ]; then
    iend=12
  else
    iend=${mon_ed}
  fi

  echo "year = ${year}  start = ${i}  end = ${iend}"

  while [ $i -le $iend ];
  do

    yr=${year}
    mn=${i}
    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )

    yyyy=${yr0}
    yyyymm=${yr0}${mn0}

    # temperature

    sed -e s%@jra_dir@%${jra_dir}% \
        -e s%@core_dir@%${core_dir}% \
        -e s%@had_dir@%${had_dir}% \
        -e s%@cobe_dir@%${cobe_dir}% \
        -e s%@corr_dir@%${corr_dir}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@iter_ocean@%${iter_ocean}% \
        -e s%@iter_all@%${iter_all}% \
        namelist.smooth_tmp_icefree_v1_4_template > namelist.smooth_tmp_icefree
    ./smooth_tmp_anom_icefree_ocean

    # specific humidity

    sed -e s%@jra_dir@%${jra_dir}% \
        -e s%@core_dir@%${core_dir}% \
        -e s%@had_dir@%${had_dir}% \
        -e s%@cobe_dir@%${cobe_dir}% \
        -e s%@corr_dir@%${corr_dir}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@iter_ocean@%${iter_ocean}% \
        -e s%@iter_all@%${iter_all}% \
        namelist.smooth_sph_icefree_v1_4_template > namelist.smooth_tmp_icefree
    ./smooth_tmp_anom_icefree_ocean

    i=$(( $i + 1 ))

  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
