#! /bin/sh
#
#  Usage: h_intpol.sh
#

flin1="../linkdir/forcing/jra_latlon/195801/tmp10m.1958010100"
flot1="../linkdir/forcing/jra_tmp/tmp10m_intpl.1958010100"

./h_intpol<<EOF
&nml_hintpol
 flin1="${flin1}",
 flot1="${flot1}",
 /
EOF
