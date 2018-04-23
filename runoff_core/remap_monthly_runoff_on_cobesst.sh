#!/bin/bash -f

set -e

ln -sf NAMELIST.MXE.COBESST.ocean_monthly NAMELIST.MXE.COBESST

./remap_on_cobesst_grid
