#! /bin/sh
#

l_make_table=.false.
nxin=288
nyin=145
fltable="$1"

fcorein=$2
fdatein=$3
nvar=$4
txyu='t'
l_rot_in=$5
fcoreout=$6


fltopo="./data/topo.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./mktable<<EOF
 &nml_mktable
 l_make_table=${l_make_table}
 nxin=${nxin}
 nyin=${nyin}
 fltable="$fltable"
 /
 &nml_iodata
 fcorein="$fcorein"
 fdatein="$fdatein"
 nvar=${nvar}
 txyu="$txyu"
 l_rot_in=${l_rot_in}
 fcoreout="$fcoreout"
 /
EOF
