#!/bin/bash -f

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

orgdir=/work116/htsujino/jra-55/Hist/Monthly/ice
newdir=/work116/htsujino/JRA-55-MONTHLY/jra-org-grid/ice

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

  echo "start month = ${m}"
  echo "  end month = ${mend}"

  while [ ${m} -le ${mend} ];
  do
    yr0=$( printf %04d $year )
    mn0=$( printf %02d $m )
    yyyymm=${yr0}${mn0}
    wgrib -s ${orgdir}/ice.${yyyymm} | egrep '(:ICEC:sfc:)' | wgrib -i -bin -nh ${orgdir}/ice.${yyyymm} -o ${newdir}/ice.${yyyymm}
    m=$(( $m + 1 ))
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
