#!/bin/bash -f

set -e

rm -f namelist.make_precip_correc

# for 2006-present

period_ref=2006_2015
period_trg=2006_2015
fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
file_ref_base=../linkdir/GPCP-v2_3/grads_monclim_adjusted_T62/precip_${period_ref}

sed -e s%@file_ref_base@%${file_ref_base}% \
    -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    namelist.make_precip_correc_v1_2_template > namelist.make_precip_correc

./mk_correc_precip

# for 1999-2006

period_ref=1999_2006
period_trg=1999_2006
fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
file_ref_base=../linkdir/CORE/core_T62_clim/prcp_${period_ref}

sed -e s%@file_ref_base@%${file_ref_base}% \
    -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    namelist.make_precip_correc_v1_2_template > namelist.make_precip_correc

./mk_correc_precip


# for 1999-2008

#period_ref=1999_2008
#period_trg=1999_2008
#fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
#file_ref_base=../linkdir/CORE/core_T62_clim/prcp_${period_ref}
#
#sed -e s%@file_ref_base@%${file_ref_base}% \
#    -e s%@period_ref@%${period_ref}% \
#    -e s%@period_trg@%${period_trg}% \
#    -e s%@fcst_clim@%${fcst_clim}% \
#    namelist.make_precip_correc_v1_2_template > namelist.make_precip_correc
#
#./mk_correc_precip
