#! /bin/sh
#  fourier.sh  
#

imax=${1}
jmax=${2}
kmax=${3}
flin="${4}"
flxout="${5}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./fourier<<EOF
 &nml_fourier
 imax=${imax}
 jmax=${jmax}
 kmax=${kmax}
 flin="${flin}"
 l_on_kx=.true.
 l_on_ky=.false.
 flxout="${flxout}"
 /
EOF

