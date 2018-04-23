#!/bin/bash -f

set -e 

file_table='../linkdir/data/red2reg_fill_with_water.d'

# JRA55anl
#orgdir=../linkdir/forcing/jra55anl_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_corrfac_v7_TL319
#period=2002_2015
#item=ocn

# JRA55anl
#orgdir=../linkdir/forcing/jra55anl_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_corrfac_v7_TL319
#period=1979_1998
#item=ice

# JRA55anl_erai
#orgdir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319
#period=1979_1998
#item=ice

# JRA55fcst_v7
#orgdir=../linkdir/forcing/jra55fcst_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55fcst_corrfac_v7_TL319
#period=1958_1972
#period=1973_1996
#period=1999_2015
#item=ocn

################

rm -f namelist.make_red2reg_tmp2m_corrfac

for surf in `echo ${item}`
do
  sed -e s%@surf@%${surf}% \
      -e s%@file_table@%${file_table}% \
      -e s%@org_dir@%${orgdir}% \
      -e s%@latlon_dir@%${latlondir}% \
      -e s%@period@%${period}% \
      namelist.make_red2reg_tmp2m_corrfac_template > namelist.make_red2reg_tmp2m_corrfac
  ./make_red2reg_tmp2m_corrfac
done

rm -f namelist.make_red2reg_tmp2m_corrfac
