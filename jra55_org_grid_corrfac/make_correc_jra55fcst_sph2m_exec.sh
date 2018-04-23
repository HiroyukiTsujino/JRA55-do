#!/bin/bash -f

set -e

rm -f namelist.make_sph2mcorrec

apply_mask=.false.

# for 1999-present

period_ref=1999_2014
period_trg=1999_2015
#fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
fcst_clim=../linkdir/forcing/jra55fcst_v7a_monclim_TL319r

sed -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    -e s%@l_apply_mask@%${apply_mask}% \
    namelist.make_sph2m_fcst_correc_template > namelist.make_sph2mcorrec
./mk_correc_sph2m

# for 1973-1996

period_ref=1980_1996
period_trg=1973_1996
#fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
fcst_clim=../linkdir/forcing/jra55fcst_v7a_monclim_TL319r

sed -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    -e s%@l_apply_mask@%${apply_mask}% \
    namelist.make_sph2m_fcst_correc_template > namelist.make_sph2mcorrec

./mk_correc_sph2m

# for 1958-1972

period_ref=1980_1996
period_trg=1958_1972
#fcst_clim=../linkdir/forcing/jra55Cfcst_monclim_TL319r
fcst_clim=../linkdir/forcing/jra55Cfcst_v7a_monclim_TL319r

sed -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    -e s%@l_apply_mask@%${apply_mask}% \
    namelist.make_sph2m_fcst_correc_template > namelist.make_sph2mcorrec

./mk_correc_sph2m
