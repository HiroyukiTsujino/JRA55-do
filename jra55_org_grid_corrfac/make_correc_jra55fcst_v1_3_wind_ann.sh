#!/bin/bash -f

set -e

######################
# 1999-2015

ref_dir=../linkdir/forcing/qscat_jra55anl_filt_annclim_TL319r
fcst_dir=../linkdir/forcing/jra55fcst_filt_v1_2tq_annclim_TL319r
period_cmp=nov1999_oct2009
period_trg=1999_2015

rm -f namelist.windcorrec_mag_offset_ann

sed -e s%@period_cmp@%${period_cmp}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@ref_dir@%${ref_dir}% \
    -e s%@fcst_dir@%${fcst_dir}% \
    namelist.windcorrec_mag_ann_fcst_filt_v1_3_template > namelist.windcorrec_mag_offset_ann

./mk_correc_wmag_offset_ann

######################
# 1973-1996

ref_dir=../linkdir/forcing/remss_jra55anl_filt_qscat_annclim_TL319r
fcst_dir=../linkdir/forcing/jra55fcst_filt_v1_2tq_annclim_TL319r
period_cmp=jan1988_dec1996
period_trg=1973_1996

rm -f namelist.windcorrec_mag_offset_ann

sed -e s%@period_cmp@%${period_cmp}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@ref_dir@%${ref_dir}% \
    -e s%@fcst_dir@%${fcst_dir}% \
    namelist.windcorrec_mag_ann_fcst_filt_v1_3_template > namelist.windcorrec_mag_offset_ann

./mk_correc_wmag_offset_ann

######################
# 1958-1972

ref_dir=../linkdir/forcing/remss_jra55anl_filt_qscat_annclim_TL319r
fcst_dir=../linkdir/forcing/jra55Cfcst_filt_v1_2tq_annclim_TL319r
period_cmp=jan1988_dec1996
period_trg=1958_1972

rm -f namelist.windcorrec_mag_offset_ann

sed -e s%@period_cmp@%${period_cmp}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@ref_dir@%${ref_dir}% \
    -e s%@fcst_dir@%${fcst_dir}% \
    namelist.windcorrec_mag_ann_fcst_filt_v1_3_template > namelist.windcorrec_mag_offset_ann

./mk_correc_wmag_offset_ann
