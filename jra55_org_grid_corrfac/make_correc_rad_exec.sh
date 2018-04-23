#!/bin/bash -f

set -e

ln -sf NAMELIST.MXE.CERES.monclim NAMELIST.MXE.CERES

######

#period_ref=mar2000_feb2015
#period_trg=1973_2015
#fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
#
#rm -f namelist.make_radcorrec
#
#for item in dswrf dlwrf
#do
#  sed -e s%@period_ref@%${period_ref}% \
#      -e s%@period_trg@%${period_trg}% \
#      -e s%@fcst_clim@%${fcst_clim}% \
#      namelist.make_${item}_correc > namelist.make_radcorrec
#  ./mk_correc_rad
#done

######
#
#period_ref=jan2001_dec2012
#period_trg=1958_1972
#fcst_clim=../linkdir/forcing/jra55Cfcst_monclim_TL319r
#
#rm -f namelist.make_radcorrec
#
#for item in dswrf dlwrf
#do
#  sed -e s%@period_ref@%${period_ref}% \
#      -e s%@period_trg@%${period_trg}% \
#      -e s%@fcst_clim@%${fcst_clim}% \
#      namelist.make_${item}_correc > namelist.make_radcorrec
#  ./mk_correc_rad
#done

######
#
period_ref=jan1979_dec1996
period_trg=1958_1972
fcst_clim=../linkdir/forcing/jra55Cfcst_monclim_TL319r

rm -f namelist.make_radcorrec_nat

for item in dswrf dlwrf
do
  sed -e s%@period_ref@%${period_ref}% \
      -e s%@period_trg@%${period_trg}% \
      -e s%@fcst_clim@%${fcst_clim}% \
      -e s%@item@%${item}% \
      namelist.make_rad_nat_jra55c_correc_template > namelist.make_radcorrec_nat
  ./mk_correc_rad_on_native
done
