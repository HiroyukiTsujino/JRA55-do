#! /bin/sh
#
#  Usage: h_intpol.sh
#
set -e

ln -sf NAMELIST.MXE.CORE    NAMELIST.MXE.ORG
ln -sf NAMELIST.MXE.COBESST NAMELIST.MXE.NEW

styear=1948
endyear=2009

year=${styear}

while [ ${year} -le ${endyear} ];
do
  echo ${year}
for m in 01 02 03 04 05 06 07 08 09 10 11 12
do
flin1="../linkdir/CORE/core_T62_monthly/prcp.${year}${m}"
flot1="../linkdir/CORE/core_1x1_monthly/prcp.${year}${m}"

./h_intpol<<EOF
&nml_hintpol
 flin1="${flin1}",
 flot1="${flot1}",
 num_data=1
 /
EOF

done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
