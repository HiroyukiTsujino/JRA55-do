#!/bin/bash -f

set -e 

blank=0.0d0

################

rm -f namelist.jra55_reg2red

ln -sf namelist.reanl_weight_reg2red_ensemble4  namelist.jra55_reg2red

./jra55reg_to_jra55red
