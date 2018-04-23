#!/bin/bash -f

yearst=${1}
yeared=${2}
mon_st=${3}
mon_ed=${4}

orgdir=/work116/htsujino/jra-55/Hist/Monthly/fcst_surf
newdir=/work116/htsujino/JRA-55-MONTHLY/jra-org-grid/fcst


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

    wgrib -s ${orgdir}/fcst_surf.${yyyymm} | egrep '(:TMP:)'   | wgrib -i -bin -nh ${orgdir}/fcst_surf.${yyyymm} -o ${newdir}/tmp2m.${yyyymm}
    wgrib -s ${orgdir}/fcst_surf.${yyyymm} | egrep '(:SPFH:)'  | wgrib -i -bin -nh ${orgdir}/fcst_surf.${yyyymm} -o ${newdir}/sph2m.${yyyymm}
    wgrib -s ${orgdir}/fcst_surf.${yyyymm} | egrep '(:UGRD:)'  | wgrib -i -bin -nh ${orgdir}/fcst_surf.${yyyymm} -o ${newdir}/u10m.${yyyymm}
    wgrib -s ${orgdir}/fcst_surf.${yyyymm} | egrep '(:VGRD:)'  | wgrib -i -bin -nh ${orgdir}/fcst_surf.${yyyymm} -o ${newdir}/v10m.${yyyymm}


    m=$(( $m + 1 ))

  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
