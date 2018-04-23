#!/bin/bash -f

set -e

rm -f namelist.make_tmp2mcorrec_fill

# for 1999-present

period_ref=1999_2014
period_trg=1999_2015
fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r
ref_all_base=../linkdir/forcing/ensemble4_monclim_TL319r

for surf in ocn ice
do
  if [ "${surf}" = "ice" ]; then
    ref_base=../linkdir/forcing/jra55anl_npoles_monclim_TL319r
  else
    ref_base=../linkdir/forcing/ensemble4_monclim_TL319r
  fi
  sed -e s%@period_ref@%${period_ref}% \
      -e s%@period_trg@%${period_trg}% \
      -e s%@ref_base@%${ref_base}% \
      -e s%@ref_all_base@%${ref_all_base}% \
      -e s%@fcst_clim@%${fcst_clim}% \
      -e s%@surf@%${surf}% \
      namelist.make_tmp2m_correc_v1_2_fill_template > namelist.make_tmp2mcorrec_fill

  ./mk_correc_tmp2m_fill_with_all
done

# for 1973-1996

period_ref=1980_1996
period_trg=1973_1996
fcst_clim=../linkdir/forcing/jra55fcst_monclim_TL319r

for surf in ocn ice
do
  if [ "${surf}" = "ice" ]; then
    ref_base=../linkdir/forcing/jra55anl_npoles_monclim_TL319r
  else
    ref_base=../linkdir/forcing/ensemble4_monclim_TL319r
  fi
  sed -e s%@period_ref@%${period_ref}% \
      -e s%@period_trg@%${period_trg}% \
      -e s%@ref_base@%${ref_base}% \
      -e s%@ref_all_base@%${ref_all_base}% \
      -e s%@fcst_clim@%${fcst_clim}% \
      -e s%@surf@%${surf}% \
      namelist.make_tmp2m_correc_v1_2_fill_template > namelist.make_tmp2mcorrec_fill

  ./mk_correc_tmp2m_fill_with_all
done

# for 1958-1972

period_ref=1980_1996
period_trg=1958_1972
fcst_clim=../linkdir/forcing/jra55Cfcst_monclim_TL319r

for surf in ocn ice
do
  if [ "${surf}" = "ice" ]; then
    ref_base=../linkdir/forcing/jra55anl_npoles_monclim_TL319r
  else
    ref_base=../linkdir/forcing/ensemble4_monclim_TL319r
  fi
  sed -e s%@period_ref@%${period_ref}% \
      -e s%@period_trg@%${period_trg}% \
      -e s%@ref_base@%${ref_base}% \
      -e s%@ref_all_base@%${ref_all_base}% \
      -e s%@fcst_clim@%${fcst_clim}% \
      -e s%@surf@%${surf}% \
      namelist.make_tmp2m_correc_v1_2_fill_template > namelist.make_tmp2mcorrec_fill

  ./mk_correc_tmp2m_fill_with_all
done
