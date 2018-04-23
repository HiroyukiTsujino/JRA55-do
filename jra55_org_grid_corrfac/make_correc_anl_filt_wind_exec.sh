#!/bin/bash -f

set -e

#if [ x${1} = x ]; then
#  echo "Usage ${0} version (v1,v2,v3)"
#  exit
#fi

#ver=${1}

#ln -sf namelist.windcorrec_ceof_ann_anl_filt namelist.windcorrec_ceof_ann
#
#./mk_correc_wvec_ceof_ann

ln -sf namelist.windcorrec_tanh_ann_anl_filt namelist.windcorrec_tanh_ann

./mk_correc_wvec_tanh_ann
