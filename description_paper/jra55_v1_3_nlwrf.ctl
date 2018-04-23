dset ^/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_annual_1x1/nlwrf.%y4
undef 9.999E+20
title JRA55 diagnosed flux monthly
options template big_endian
ydef 180 linear -89.500000 1.000000
xdef 360 linear   0.500000 1.000000
tdef 720 linear jan1958 1yr
zdef 1 linear 1 1
vars 1
nlwrf  0 99 net long wave radiation [W/m2] 
ENDVARS
