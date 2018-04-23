#! /bin/sh
#

l_make_table=.true.
fgrid_info=$1
file_vgrid="data/vgrid.d"
nxin=721
nyin=577
nlat=3
nlon=4
fltable="$2"


fcorein=$3
fdatein=$4
nvaru=1
nvarv=2
txyu="u"
fcoreout=$5


F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mktable<<EOF
 &nml_mktable
 l_make_table=${l_make_table}
 fgrid_info="$fgrid_info"
 file_vgrid="$file_vgrid"
 nxin=${nxin}
 nyin=${nyin}
 nlat=${nlat}
 nlon=${nlon}
 fltable="$fltable"
 /
 &nml_iodata
 fcorein="$fcorein"
 fdatein="$fdatein"
 nvaru=${nvaru}
 nvarv=${nvarv}
 txyu="$txyu"
 fcoreout="$fcoreout"
 /
EOF

