#!/bin/sh -f

dayst=${1}
dayed=${2}

yearorg=1901
yearnew=1901

klevel=10
item=hs_t

source ~/ocsv001_home/bin/datel_noleap.sh

################
pardir=/workd/htsujino/BC_VORTEX_NEST/e011/result_parent
subdir=/workd/htsujino/BC_VORTEX_NEST/e011/result_sub
outdir=/workd/htsujino/BC_VORTEX_NEST/e011/result_mabiki
################

numday=${dayst}

while [ ${numday} -le ${dayed} ];
do
  nydate ${yearorg} ${numday}
  echo ${iyear} ${mon} ${iday}

#  for hour in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23
#  do

    if [ ${mon} -lt 10 ]; then
      month=0${mon}
    else
      month=${mon}
    fi

    if [ ${iday} -lt 10 ]; then
      day=0${iday}
    else
      day=${iday}
    fi

#    exto=${yearorg}${month}${day}${hour}00
#    extn=${yearnew}${month}${day}${hour}00
    exto=${yearorg}${month}${day}
    extn=${yearnew}${month}${day}

    echo ${exto} ${extn}

    hs_par=${pardir}/${item}.${exto}
    hs_sub=${subdir}/${item}.${exto}
    hs_rep=${outdir}/${item}.${extn}
    replace_with_zero=.false.

    sed -e "
    s@%hs_par%@${hs_par}@g
    s@%hs_sub%@${hs_sub}@g
    s@%hs_rep%@${hs_rep}@g
    s@%replace_with_zero%@${replace_with_zero}@g
    s@%klevel%@${klevel}@g
    " < namelist.mabiki_template > namelist.mabiki

    ./mabiki

#  done

  numday=`expr ${numday} + 1`

done
