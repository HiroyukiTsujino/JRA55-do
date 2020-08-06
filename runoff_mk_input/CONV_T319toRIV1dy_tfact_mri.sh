#!/bin/bash

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} start_year end_year (start_day end_day)"
   exit
fi

yrst=${1}
yred=${2}
dyst=${3}
dyed=${4}

indir=../linkdir/work/jra55fcst_v1_3_input_runoff_3hr_TL319r
outdir=../linkdir/work/jra55fcst_v1_3_input_runoff_1dy_025x025
facdir=../linkdir/corrfac/v1_3/jra55fcst_corrfac_v1_3_input_runoff_025x025
mskdir=../linkdir/work/const

mask=${mskdir}/TL319.mask.withheader

if [ ! -e ${outdir} ]; then
  echo "creating ${outdir}"
  mkdir ${outdir}
fi

################

. ../util/datel_leap.sh

year=${yrst}

while [ ${year} -le ${yred} ];

do
  echo ${year}
  leap=`isleap ${year}`
  if [ x${dyst} == x ]; then
    i=1
  else
    i=${dyst}
  fi
  if [ x${dyed} == x ]; then
    iend=`expr 365 + ${leap}`
  else
    iend=${dyed}
  fi

  echo "iend = ${iend}"

##################################################

  while [ ${i} -le ${iend} ];
  do
    yr=$( nydate $year $i | awk '{print $1 }' )
    mn=$( nydate $year $i | awk '{print $2 }' )
    dy=$( nydate $year $i | awk '{print $3 }' )

    yr0=$( printf %04d $yr )
    mn0=$( printf %02d $mn )
    dy0=$( printf %02d $dy )

    yyyymm=${yr0}${mn0}
    yyyymmdd=${yr0}${mn0}${dy0} 

    infile=${indir}/watr${yyyymmdd}
    outfile=${outdir}/rof${yyyymmdd}.grd

    if [ ${year} -le 2016 ]; then
      yearm=${year}
    else
      yearm=2016
    fi

    yyyymmddm=${yearm}${mn0}${dy0} 
    fact=${facdir}/tfact${yyyymmddm}.grd

    echo " Correcting factor read from ${fact} "

    if [ -e ${infile} ]; then
      echo " Input : ${infile}"
      echo " Output: ${outfile}"
      rm -f namelist
      echo " &nmlst ifile='${infile}',ofile='${outfile}',mfile='${mask}',fact=t, factor='${fact}' &end " > namelist
      ./CONV_T319toRIV1dy_tfact/src/conv_riv < ./namelist
      i=$(( $i + 1 ))
    else
      i=$(( ${iend} + 1 ))
    fi
  done

  year=$(( ${year} + 1 ))

done
