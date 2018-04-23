#! /bin/sh
#  fourier.sh  
#

imax=${1}
jmax=${2}
kmax=${3}
flin="${4}"
j_offset=${5}
nymax=${6}
flyout="${7}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./fourier<<EOF
 &nml_fourier
 imax=${imax}
 jmax=${jmax}
 kmax=${kmax}
 flin="${flin}"
 l_on_kx=.false.
 l_on_ky=.true.
 j_offset=${j_offset}
 nymax=${nymax}
 flyout="${flyout}"
 /
EOF

