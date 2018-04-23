#!/bin/bash -f

for item in all ice
do
  ln -sf namelist.make_tmp2m_${item}_correc namelist.make_tmp2mcorrec
 ./mk_correc_tmp2m
done

rm -f namelist.make_tmp2mcorrec
