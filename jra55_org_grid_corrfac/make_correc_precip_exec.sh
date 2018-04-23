#!/bin/bash -f

set -e

rm -f namelist.make_precip_correc

# for 1999-present

period_ref=1999_2009
period_trg=1999_2015
fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r

sed -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    namelist.make_precip_correc_template > namelist.make_precip_correc

./mk_correc_precip

# for 1973-1996

period_ref=1979_1996
period_trg=1973_1996
fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r

sed -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    namelist.make_precip_correc_template > namelist.make_precip_correc

./mk_correc_precip

# for 1958-1972

period_ref=1979_1996
period_trg=1958_1972
fcst_clim=../linkdir/forcing/jra55Cfcst_monclim_TL319r

sed -e s%@period_ref@%${period_ref}% \
    -e s%@period_trg@%${period_trg}% \
    -e s%@fcst_clim@%${fcst_clim}% \
    namelist.make_precip_correc_template > namelist.make_precip_correc

./mk_correc_precip
