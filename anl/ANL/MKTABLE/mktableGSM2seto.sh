#! /bin/sh
#

l_make_table=.true.
l_read_latlon=.false.
#file_vgrid="data/vgrid.d"
nxin=288
nyin=145
slat=-90.0
slon=0.0
dlat=1.25
dlon=1.25
fltable="$1"


fcorein=$2
fdatein=$3
nvaru=2
nvarv=3
txyu="u"
fcoreout=$4


F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mktable<<EOF
 &nml_mktable
 l_make_table=${l_make_table}
 l_read_latlon=${l_read_latlon}
 nxin=${nxin}
 nyin=${nyin}
 slat=${slat}
 slon=${slon}
 dlat=${dlat}
 dlon=${dlon}
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

