#!/bin/bash -f

set -e 

yearst=2000
yeared=2008
mon_st=1
mon_ed=12

out_weight=.false.
file_table='../linkdir/data/red2reg_fill_with_water.d'

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

#    for item in u10m_free_actual 
#    for item in v10m_free_actual wind_free_actual 
    for item in u10m_free_actual v10m_free_actual wind_free_actual 
    do

      fcst_org='/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_4_monthly_TL319r'
      fcst_latlon='/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_4_monthly_TL319'
      item_weight=${item}'_weight'

      sed -e s%@base_dir_org@%${fcst_org}% \
          -e s%@base_dir_latlon@%${fcst_latlon}% \
          -e s%@yyyymm@%${yyyymm}% \
          -e s%@item@%${item}% \
          -e s%@item_weight@%${item_weight}% \
          -e s%@ext@%${yyyymm}% \
          -e s%@file_table@%${file_table}% \
          -e s%@out_weight@%${out_weight}% \
          namelist.make_red2reg_monthly_qscat_template > namelist.make_red2reg

      ./make_red2reg_with_miss

    done

    i=$(( $i + 1 ))
    
  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
