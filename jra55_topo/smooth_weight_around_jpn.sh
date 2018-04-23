#!/bin/bash

set -e

exe=./smooth_weight

make ${exe}

rm -f NAMELIST.MXE
ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE

rm -f namelist.smooth_weight
ln -sf namelist.smooth_weight_jpn namelist.smooth_weight

./${exe}

rm -f NAMELIST.MXE
rm -f namelist.smooth_weight

exit 0
