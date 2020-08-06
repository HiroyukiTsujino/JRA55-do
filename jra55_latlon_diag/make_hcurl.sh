#!/bin/bash -f

set -e

#if [ x${1} == x ]; then
#   echo "Usage: ./make_hcurl.sh"
#   exit
#fi

ln -sf NAMELIST.MXE.JRA55_ocean_monthly NAMELIST.MXE

#indir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_annclim_TL319
#indir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_1_annclim_TL319
#indir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_2_annclim_TL319
#indir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_3_annclim_TL319
#indir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_4_annclim_TL319
#indir=/work113/htsujino/SURF_FLUX/forcing/gcurr_on_qscat_annclim_TL319

#file_x=un10m_nov1999-oct2009.dat
#file_y=vn10m_nov1999-oct2009.dat
#file_x=u10m_free_actual.nov1999-oct2009.dat
#file_y=v10m_free_actual.nov1999-oct2009.dat
#file_x=usurf_gcurr.nov1999-oct2009.dat
#file_y=vsurf_gcurr.nov1999-oct2009.dat

#outdir=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_annclim_TL319
#outdir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_1_annclim_TL319
#outdir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_2_annclim_TL319
#outdir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_3_annclim_TL319
#outdir=/work113/htsujino/SURF_FLUX/forcing/qscat_strict_free_4_annclim_TL319
#outdir=/work113/htsujino/SURF_FLUX/forcing/gcurr_on_qscat_annclim_TL319

#file_out=curl_uvn_nov1999-oct2009.dat
#file_out=curl_uv_free_actual_nov1999-oct2009.dat
#file_out=curl_uvsurf_gcurr_nov1999-oct2009.dat

################

sed -e s%@file_x@%${file_x}% \
    -e s%@file_y@%${file_y}% \
    -e s%@indir@%${indir}% \
    -e s%@file_out@%${file_out}% \
    -e s%@outdir@%${outdir}% \
namelist.curl_tau_clim_template > namelist.curl_tau

./curltau
