#!/bin/bash -f

set -e

#
item=all
l_apply_mask=.false.
sed -e s%@surf@%${item}% \
    -e s%@l_apply_mask@%${l_apply_mask}% \
    namelist.make_sph2m_anl_correc_template > namelist.make_sph2mcorrec

 ./mk_correc_sph2m

#
item=ocn
l_apply_mask=.true.
sed -e s%@surf@%${item}% \
    -e s%@l_apply_mask@%${l_apply_mask}% \
    namelist.make_sph2m_anl_correc_template > namelist.make_sph2mcorrec

 ./mk_correc_sph2m

#
item=ice
l_apply_mask=.true.
sed -e s%@surf@%${item}% \
    -e s%@l_apply_mask@%${l_apply_mask}% \
    namelist.make_sph2m_anl_correc_template > namelist.make_sph2mcorrec

 ./mk_correc_sph2m
