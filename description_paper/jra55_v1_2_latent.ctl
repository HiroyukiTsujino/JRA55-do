dset ^/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_2_annual_1x1/latent.%y4
undef 9.999E+20
title JRA55 diagnosed flux monthly
options template big_endian
ydef 180 linear -89.500000 1.000000
xdef 360 linear   0.500000 1.000000
tdef 720 linear jan1958 1yr
zdef 1 linear 1 1
vars 1
latent  0 99 latent heat flux [W/m2] 
ENDVARS
