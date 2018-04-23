#!/bin/sh

set -e

ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE

################

ftablev="../linkdir/table.05/jra55o2cobe_vec.d"
ftables="../linkdir/table.05/jra55o2cobe_scl.d"
farea="../linkdir/table.05/rmp_jra55o2cobe_area_vec.gd"
flinu="../linkdir/forcing/jra55fcst_3hr_TL319/197901/u10m.1979010100"
flinv="../linkdir/forcing/jra55fcst_3hr_TL319/197901/v10m.1979010100"
floutu="../linkdir/table.05/u10m_ocean.1979010100"
floutv="../linkdir/table.05/v10m_ocean.1979010100"

./remap_vector.sh ${flinu} ${flinv} ${floutu} ${floutv} 1 1 1 1 -9.99d33 .false. .false. .true. .true. ${ftablev} ${ftables} ${farea}

rm -f NAMELIST.MXE
