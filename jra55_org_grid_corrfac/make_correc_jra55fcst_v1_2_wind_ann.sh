#!/bin/bash -f

set -e

######################
# 1999-2015

# rotation

rm -f namelist.windcorrec_ceof_ann
ln -sf namelist.windcorrec_ceof_ann_fcst_filt_v1_2 namelist.windcorrec_ceof_ann
./mk_correc_wvec_ceof_ann

# magnitude

rm -f namelist.windcorrec_tanh_ann
ln -sf namelist.windcorrec_tanh_ann_fcst_filt_v1_2 namelist.windcorrec_tanh_ann
./mk_correc_wvec_tanh_ann

######################
# 1973-1996

fcst_dir=../linkdir/forcing/jra55fcst_filt_v1_2tq_annclim_TL319r
period_cmp=jan1988_dec1996
period_trg=1973_1996

rm -f namelist.windcorrec_mag_ann

sed -e s%@period_cmp@%${period_cmp}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_dir@%${fcst_dir}% \
    namelist.windcorrec_mag_ann_fcst_filt_v1_2_template > namelist.windcorrec_mag_ann

./mk_correc_wmag_ann

######################
# 1958-1972

fcst_dir=../linkdir/forcing/jra55Cfcst_filt_v1_2tq_annclim_TL319r
period_cmp=jan1988_dec1996
period_trg=1958_1972

rm -f namelist.windcorrec_mag_ann

sed -e s%@period_cmp@%${period_cmp}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_dir@%${fcst_dir}% \
    namelist.windcorrec_mag_ann_fcst_filt_v1_2_template > namelist.windcorrec_mag_ann

./mk_correc_wmag_ann
