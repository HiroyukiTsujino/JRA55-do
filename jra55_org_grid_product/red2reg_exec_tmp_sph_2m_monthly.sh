#!/bin/bash -f

set -e 

if [ x${2} = x ]; then
  echo "Usage: red2reg_exec_tmp_sph_2m_monthly.sh start_year end_year (start_month end_month)"
  exit
fi

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

out_weight=.false.
file_table='../linkdir/data/red2reg_fill_with_water.d'

# JRA-55 fcst
orgdir=../linkdir/forcing/jra_monthly_org
latlondir=../linkdir/forcing/jra_monthly_latlon

# JRA-55 anl
#orgdir=../linkdir/forcing/jra_anl_monthly_org
#latlondir=../linkdir/forcing/jra_anl_monthly_latlon

# JRA-55C
#orgdir=../linkdir/forcing/jra55c_monthly_org
#latlondir=../linkdir/forcing/jra55c_monthly_latlon

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

    # tmp2m

    item=tmp2m
    fcst_org="${orgdir}"
    fcst_latlon="${latlondir}"
    item_weight=${item}'_weight'

    sed -e s%@base_dir_org@%${fcst_org}% \
        -e s%@base_dir_latlon@%${fcst_latlon}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@item@%${item}% \
        -e s%@item_weight@%${item_weight}% \
        -e s%@ext@%${yyyymm}% \
        -e s%@file_table@%${file_table}% \
        -e s%@out_weight@%${out_weight}% \
        namelist.make_red2reg_monthly_template > namelist.make_red2reg

    ./make_red2reg

    # sph2m

    item=sph2m
    fcst_org="${orgdir}"
    fcst_latlon="${latlondir}"
    item_weight=${item}'_weight'

    sed -e s%@base_dir_org@%${fcst_org}% \
        -e s%@base_dir_latlon@%${fcst_latlon}% \
        -e s%@yyyymm@%${yyyymm}% \
        -e s%@item@%${item}% \
        -e s%@item_weight@%${item_weight}% \
        -e s%@ext@%${yyyymm}% \
        -e s%@file_table@%${file_table}% \
        -e s%@out_weight@%${out_weight}% \
        namelist.make_red2reg_monthly_template > namelist.make_red2reg

    ./make_red2reg

    i=$(( $i + 1 ))
    
  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
