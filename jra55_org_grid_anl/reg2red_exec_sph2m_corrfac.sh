#!/bin/bash -f

set -e 

# JRA55anl
#orgdir=../linkdir/forcing/jra55anl_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_corrfac_v7_TL319
#period=2002_2015
#item=ocn

# JRA55anl_erai
#orgdir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319
#period=1979_1998
#item=ice

blank=1.0d0

################

rm -f namelist.jra55_reg2red

for surf in `echo ${item}`
do
  sed -e s%@surf@%${surf}% \
      -e s%@org_dir@%${orgdir}% \
      -e s%@latlon_dir@%${latlondir}% \
      -e s%@period@%${period}% \
      -e s%@blank_value@%${blank}% \
      namelist.sphcorr_jra55_reg2red_template > namelist.jra55_reg2red
  ./jra55reg_to_jra55red
done
