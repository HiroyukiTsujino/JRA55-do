#! /bin/sh
#
#  Usage: h_intpol.sh
#
set -e

flin1="/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/dswrf.1948"
flot1="/worke/htsujino/CORE/core_tmp/dswrf_intpl.19480101"

./h_intpol<<EOF
&nml_hintpol
 flin1="${flin1}",
 flot1="${flot1}",
 /
EOF
