#!/bin/bash -f

set -e 

file_table='../linkdir/data/red2reg_fill_with_water.d'

# JRA55anl
#orgdir=../linkdir/forcing/jra55anl_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_corrfac_v7_TL319
#period=2002_2015
#surf=ocn

# JRA55anl
orgdir=../linkdir/forcing/jra55anl_corrfac_v7_TL319r
latlondir=../linkdir/forcing/jra55anl_corrfac_v7_TL319
period=1979_1998
surf=ice

# JRA55anl_erai
#orgdir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319r
#latlondir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319
#period=1979_1998
#surf=ice

################

rm -f namelist.make_red2reg_tmp2m_corrfac

sed -e s%@org_dir@%${orgdir}% \
    -e s%@latlon_dir@%${latlondir}% \
    -e s%@surf@%${surf}% \
    -e s%@period@%${period}% \
    namelist.make_red2reg_tmp2m_corrfac_check_template > namelist.make_red2reg_tmp2m_corrfac

./make_red2reg_tmp2m_corrfac

rm -f namelist.make_red2reg_tmp2m_corrfac
