#! /bin/sh
#

l_make_table=.false.
file_vgrid="data/vgrid.d"
nxin=288
nyin=145
fltable="$1"

fcorein=$2
fdatein=$3
nvaru=$4
nvarv=$5
txyu='u'
l_rot_in=$6
fcoreout=$7


fltopo="./data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mktable<<EOF
 &nml_mktable
 l_make_table=${l_make_table}
 file_vgrid="$file_vgrid"
 nxin=${nxin}
 nyin=${nyin}
 fltable="$fltable"
 /
 &nml_iodata
 fcorein="$fcorein"
 fdatein="$fdatein"
 nvaru=${nvaru}
 nvarv=${nvarv}
 txyu="$txyu"
 l_rot_in=${l_rot_in}
 fcoreout="$fcoreout"
 /
EOF

