#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}
item_reanl=${3}
item=${4}

mon_st=1
mon_ed=12

reanl_dir='../linkdir/NCEP_R2/grads_monthly'
reanl_jra='../linkdir/forcing/ncep2_monthly_TL319r'
reanl_jra_latlon='../linkdir/forcing/ncep2_monthly_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'
reanl_cnst='../linkdir/NCEP_R2/grads_const'
file_mxe_reanl='NAMELIST.MXE.NCEP2'
file_mask_reanl='lsmask.gd'
l_little_in=.false.

scale_in=1.0d0
offset_in=0.0d0

flip_mask_reanl=.false.
mask_thre_reanl=1.0d0,
out_latlon=.true.
undef_reanl=-9.99e33
undef_out=9.999e20

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
    sed -e s%@yyyymm@%${yyyymm}% \
        -e s%@jra_cnst@%${jra_cnst}% \
        -e s%@reanl_cnst@%${reanl_cnst}% \
        -e s%@reanl_jra@%${reanl_jra}% \
        -e s%@reanl_jra_latlon@%${reanl_jra_latlon}% \
        -e s%@reanl_dir@%${reanl_dir}% \
        -e s%@item@%${item}% \
        -e s%@item_reanl@%${item_reanl}% \
        -e s%@irec@%1% \
        -e s%@file_mxe_reanl@%${file_mxe_reanl}% \
        -e s%@file_mask_reanl@%${file_mask_reanl}% \
        -e s%@flip_mask_reanl@%${flip_mask_reanl}% \
        -e s%@mask_thre_reanl@%${mask_thre_reanl}% \
        -e s%@little_in@%${l_little_in}% \
        -e s%@scale_in@%${scale_in}% \
        -e s%@offset_in@%${offset_in}% \
        -e s%@out_latlon@%${out_latlon}% \
	-e s%@undef_reanl@%${undef_reanl}% \
	-e s%@undef_out@%${undef_out}% \
        namelist.reanl_on_jra55_template > namelist.reanl_on_jra55
    ./reanl_on_jra55_grid
    m=$(( $m + 1 ))
  done

  yearn=`expr ${year} + 1`
  year=${yearn}

done
