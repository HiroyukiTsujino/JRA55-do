#!/bin/bash -f

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

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
    sed -e s/@yyyymm@/${yyyymm}/ \
        namelist.blend_iabp_anl_template > namelist.blend_iabp_anl
    ./blend_iabp_npoles_anl
    m=$(( $m + 1 ))
  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
