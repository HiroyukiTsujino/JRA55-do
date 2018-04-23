#! /bin/sh
#  fourier2.sh  
#

imax=${1}
jmax=${2}
kmax=${3}
k_ref=${4}
j_stt=${5}
nymax=${6}
flin="${7}"
flout="${8}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./fourier2<<EOF
 &nml_fourier2
 imax=${imax}
 jmax=${jmax}
 kmax=${kmax}
 flin="${flin}"
 flout="${flout}"
 k_ref=${k_ref}
 j_stt=${j_stt}
 nymax=${nymax}
 /
EOF

