#!/bin/sh

set -e

ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE

################

ftable="../linkdir/table.04/jra55o2cobe.d"
farea="../linkdir/table.04/rmp_jra55o2cobe_area.gd"
flin="../linkdir/forcing/jra55fcst_3hr_TL319/197901/tmp2m.1979010100"
flout="../linkdir/table.04/tmp2m_ocean.1979010100"

#ftable="../linkdir/table.03/jra55o2cobe.d"
#farea="../linkdir/table.03/rmp_jra55o2cobe_area.gd"
#flin="../linkdir/forcing/jra55fcst_3hr_TL319/197901/tmp2m.1979010100"
#flout="../linkdir/table.03/tmp2m_ocean.1979010100"

#./remap_scalar.sh ${flin} ${flout} u .false. .true. 1 1 1 -9.99d33 .true. .false. ${ftable} ${farea} .false.
./remap_scalar.sh ${flin} ${flout} u .false. .true. 1 1 1 -9.99d33 .false. .true. ${ftable} ${farea} .false.

rm -f NAMELIST.MXE
