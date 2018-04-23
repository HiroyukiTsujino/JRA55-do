#!/bin/bash -f

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE.COBESST

ln -sf namelist.riv2onedeg-v1_1 namelist.riv2onedeg

./create_1x1_monthly
