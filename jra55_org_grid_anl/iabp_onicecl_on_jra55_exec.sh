#!/bin/bash -f

set -e

item_iabp=tair_ice.1979_1998
item=tmp2m_ice.1979_1998

mon_st=1
mon_ed=12

iabp_dir='/work116/htsujino/IABP_NPOLES/grads_moncl'
iabp_jra='../linkdir/forcing/npoles_monclim_TL319r'
iabp_jra_latlon='../linkdir/forcing/npoles_monclim_TL319'
jra_cnst='../linkdir/forcing/jra_org/const'

out_latlon=.true.
undef_out=9.999e20

################

for m in `seq -w 1 12`
do
  sed -e s%@yyyymm@%m${m}% \
      -e s%@jra_cnst@%${jra_cnst}% \
      -e s%@iabp_jra@%${iabp_jra}% \
      -e s%@iabp_jra_latlon@%${iabp_jra_latlon}% \
      -e s%@iabp_dir@%${iabp_dir}% \
      -e s%@item@%${item}% \
      -e s%@item_iabp@%${item_iabp}% \
      -e s%@irec@%1% \
      -e s%@out_latlon@%${out_latlon}% \
      -e s%@undef_out@%${undef_out}% \
      namelist.iabp_on_jra55_template > namelist.iabp_on_jra55
  ./iabp_npoles_on_jra55_grid
done
