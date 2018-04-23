#!/bin/bash -f

yyyymmddhh=${1}
yyyymm=${2}

orgdir=../linkdir/forcing/fcst_surf
newdir=../linkdir/forcing/jra_org

if [ ! -e ${newdir}/${yyyymm} ]; then
  echo "creating ${yyyymm}"
  mkdir ${newdir}/${yyyymm}
fi
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:PRMSL:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/slprs.${yyyymmddhh}
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:TMP:)'   | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/tmp2m.${yyyymmddhh}
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:SPFH:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/sph2m.${yyyymmddhh}
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:UGRD:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/u10m.${yyyymmddhh}
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:VGRD:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/v10m.${yyyymmddhh}
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:BRTMP:)' | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/brtmp.${yyyymmddhh}
wgrib -s ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} | egrep '(:SFCR:)'  | wgrib -i -bin -nh ${orgdir}/${yyyymm}/fcst_surf.${yyyymmddhh} -o ${newdir}/${yyyymm}/zrough.${yyyymmddhh}
