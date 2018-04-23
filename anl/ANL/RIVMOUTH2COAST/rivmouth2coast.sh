#! /bin/sh
#

l_initial=${1}
file_point="${2}"
plon=${3}
plat=${4}
nmax_pdata=${5}
file_coast="${6}"
nmax_coast=${7}
file_out="${8}"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./rivmouth2coast<<EOF
 &nml_rivmouth2coast
  l_initial=${l_initial},
  file_point="${file_point}",
  plon=${plon},
  plat=${plat},
  nmax_pdata=${nmax_pdata},
  file_coast="${file_coast}",
  nmax_coast=${nmax_coast},
  file_out="${file_out}"
 /
EOF

