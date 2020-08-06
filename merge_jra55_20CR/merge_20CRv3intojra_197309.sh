#!/bin/bash

set -e

# note overwrite is not allowed
# please do in one operation

#item_jra55="u10m"; item_20CR="uwnd.10m"; item_out="u10m"
#item_jra55="v10m"; item_20CR="vwnd.10m"; item_out="v10m"
#item_jra55="slprs"; item_20CR="prmsl"; item_out="slprs"
#item_jra55="tmp10m"; item_20CR="tmp10m"; item_out="tmp10m"
#item_jra55="sph10m"; item_20CR="sph10m"; item_out="sph10m"
#item_jra55="dswrf"; item_20CR="dswrf.sfc"; item_out="dswrf"
#item_jra55="dlwrf"; item_20CR="dlwrf.sfc"; item_out="dlwrf"
#item_jra55="prcp"; item_20CR="prate"; item_out="prcp"

for tc in 11L_00 11L_01 11L_02 13L_00
do
  echo ${tc}
  sed -e s%@item_jra55@%${item_jra55}% \
      -e s%@item_20CR@%${item_20CR}% \
      -e s%@item_out@%${item_out}% \
      namelist/namelist.merge20CRv3jra55_1973_${tc}_template > namelist.merge20CRv3jra55

  cat namelist.merge20CRv3jra55
  ./merge_20CRv3_and_jra

  rm -f namelist.merge20CRv3jra55
done
