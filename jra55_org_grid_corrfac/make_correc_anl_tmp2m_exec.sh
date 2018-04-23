#!/bin/bash -f

set -e

period=1979_1998
#period=2002_2015

#
#item=all
#l_apply_mask=.false.
#sed -e s%@surf@%${item}% \
#    -e s%@l_apply_mask@%${l_apply_mask}% \
#    namelist.make_tmp2m_anl_correc_template > namelist.make_tmp2mcorrec
#
# ./mk_correc_tmp2m
#
#
#item=ocn
#l_apply_mask=.true.
#sed -e s%@surf@%${item}% \
#    -e s%@l_apply_mask@%${l_apply_mask}% \
#    namelist.make_tmp2m_anl_correc_template > namelist.make_tmp2mcorrec
#
# ./mk_correc_tmp2m
#
#
item=ice
l_apply_mask=.true.
sed -e s%@surf@%${item}% \
    -e s%@period@%${period}% \
    -e s%@l_apply_mask@%${l_apply_mask}% \
    namelist.make_tmp2m_anl_correc_template > namelist.make_tmp2mcorrec

 ./mk_correc_tmp2m
