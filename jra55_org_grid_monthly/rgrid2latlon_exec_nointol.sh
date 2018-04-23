#!/bin/bash -f

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}
anltype=${5}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  if [ x${mon_st} == x ]; then
    m=1
  else
    m=${mon_st}
  fi

  if [ x${mon_ed} == x ]; then
    mend=12
  else
    mend=${mon_ed}
  fi

  while [ $m -le $mend ];
  do

    yr0=$( printf %04d $year )
    mn0=$( printf %02d $m )

    yyyymm=${yr0}${mn0}

    for item in ice
    do
      sed -e s/@yyyymm@/${yyyymm}/ \
          -e s/@type@/${anltype}/ \
          -e s/@item@/${item}/ \
            namelist.rg2latlon_nointpol_template > namelist.rg2latlon_nointpol
        ./rgrid2latlon_nointpol
    done

    m=$(( $m + 1 ))

  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
