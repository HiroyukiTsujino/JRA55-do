#!/bin/bash -f

yyyymmddhh=${1}
yyyymm=${2}

orgdir=../linkdir/forcing/fcst_phy2m
newdir=../linkdir/forcing/jra_org

if [ ! -e ${newdir}/${yyyymm} ]; then
  echo "creating ${yyyymm}"
  mkdir ${newdir}/${yyyymm}
fi

#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:DSWRF:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/dswrf.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:DLWRF:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/dlwrf.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:USWRF:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/uswrf.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:ULWRF:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/ulwrf.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:APCP:sfc:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/prcp.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:EVP:sfc:)'   | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/evap.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:SHTFL:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/sensible.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:LHTFL:sfc:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/latent.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:UFLX:sfc:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/uflx.${yyyymmddhh}
#wgrib -s ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} | egrep '(:VFLX:sfc:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_phy2m.${yyyymmddhh} -o ${newdir}/${yyyymm}/vflx.${yyyymmddhh}
