#!/bin/bash -f

if [ x${1} = x ]; then
  echo "Usage ${0} version (v1,v2,v3)"
  exit
fi

ver=${1}

ln -sf namelist.windcorrec_ceof_mon_${ver} namelist.windcorrec_ceof_mon

./mk_correc_wvec_ceof_mon

./mk_correc_wvec_tanh_mon
