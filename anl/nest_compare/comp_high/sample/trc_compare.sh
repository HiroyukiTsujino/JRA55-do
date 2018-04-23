#! /bin/sh -f

dayst=${1}
dayed=${2}

year=1901

klevel=1
item=hs_ssh

source ~/ocsv001_home/bin/datel_noleap.sh

################
rgndir=/workd/htsujino/BC_VORTEX_NEST/e011/result_sub
alldir=/worka/htsujino/BC_VORTEX/result_e002
outdir=/workd/htsujino/BC_VORTEX_NEST/e011/result_sub
################

numday=${dayst}

while [ ${numday} -le ${dayed} ];
do
  nydate ${year} ${numday}
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

#    extn=${year}${month}${day}${hour}00
    extn=${year}${month}${day}

    echo ${extn}

    hs_rgn=${rgndir}/${item}.${extn}
    hs_all=${alldir}/${item}.${extn}
    hs_rms=${outdir}/${item}_rms.${extn}

    sed -e "
    s@%hs_rgn%@${hs_rgn}@g
    s@%hs_all%@${hs_all}@g
    s@%hs_rms%@${hs_rms}@g
    s@%klevel%@${klevel}@g
    " < namelist.comp_high_template > namelist.comp_high

    ./comp_high

#  done

  numday=`expr ${numday} + 1`

done
