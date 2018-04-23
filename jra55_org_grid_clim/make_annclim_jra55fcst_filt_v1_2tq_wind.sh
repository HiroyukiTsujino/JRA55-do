#!/bin/bash -f

set -e

#nbyr=1999
#nbmn=11
#neyr=2009
#nemn=10
#period=nov1999_oct2009

#nbyr=1988
#nbmn=1
#neyr=1996
#nemn=12
#period=jan1988_dec1996

#nbyr=1973
#nbmn=1
#neyr=1996
#nemn=12
#period=jan1973_dec1996

in_dir=../linkdir/forcing/jra55fcst_filt_v1_2tq_monthly_TL319r
out_dir_org=../linkdir/forcing/jra55fcst_filt_v1_2tq_annclim_TL319r
out_dir_latlon=../linkdir/forcing/jra55fcst_filt_v1_2tq_annclim_TL319

rm -f namelist.make_annclim

for item in wn10m un10m vn10m
do
  sed -e s%@item@%${item}% \
      -e s%@in_dir@%${in_dir}% \
      -e s%@out_dir_org@%${out_dir_org}% \
      -e s%@out_dir_latlon@%${out_dir_latlon}% \
      -e s%@nbyr@%${nbyr}% \
      -e s%@nbmn@%${nbmn}% \
      -e s%@neyr@%${neyr}% \
      -e s%@nemn@%${nemn}% \
      -e s%@period@%${period}% \
      namelist.make_annclim_fcst_filt_wind_template > namelist.make_annclim
  ./make_annclim
done
