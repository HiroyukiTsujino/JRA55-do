#!/bin/sh

set -e

ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE

################

ftable="../linkdir/table.04/jra55a2cobe.d"
farea="../linkdir/table.04/rmp_jra55a2cobe_area.gd"
flin="../linkdir/forcing/jra55fcst_3hr_TL319/197901/tmp2m.1979010100"
flout="../linkdir/table.04/tmp2m_all.1979010100"

#table="../linkdir/table.03/jra55a2cobe.d"
#farea="../linkdir/table.03/rmp_jra55a2cobe_area.gd"
#flin="../linkdir/forcing/jra55fcst_3hr_TL319/197901/tmp2m.1979010100"
#flout="../linkdir/table.03/tmp2m_all.1979010100"

#./remap_scalar.sh ${flin} ${flout} u .false. .false. 1 1 1 -9.99d33 .true. .false. ${ftable} ${farea} .false.
./remap_scalar.sh ${flin} ${flout} u .false. .false. 1 1 1 -9.99d33 .false. .true. ${ftable} ${farea} .false.

rm -f NAMELIST.MXE
