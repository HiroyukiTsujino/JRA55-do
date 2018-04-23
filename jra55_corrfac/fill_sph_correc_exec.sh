#!/bin/bash -f

set -e 

# JRA55anl

#workdir=../linkdir/forcing/jra55anl_corrfac_v7_TL319
#period=2002_2015
#num_pass=20
#item=ocn

# JRA55anl_erai

#workdir=../linkdir/forcing/jra55anl_erai_corrfac_v7_TL319
#period=1979_1998
#num_pass=20
#item=ice

################

rm -f namelist.fill_mult_correc

for surf in `echo ${item}`
do
  sed -e s%@surf@%${surf}% \
      -e s%@work_dir@%${workdir}% \
      -e s%@period@%${period}% \
      -e s%@num_pass@%${num_pass}% \
      namelist.fill_sph_correc_template > namelist.fill_mult_correc
  ./fill_multiply_correc
done

