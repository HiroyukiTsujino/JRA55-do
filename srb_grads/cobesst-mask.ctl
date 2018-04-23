dset ^../linkdir/COBESST/data/cobesst-mask.gd
undef -9.99e33
title COBE-SST daily
options template big_endian
ydef 180 linear -89.500000 1.000000
xdef 360 linear 0.500000 1.000000
tdef   1 linear 00Z01jan1958 1dy
zdef 1 linear 1 1
vars 2
mask  0 99  land-sea mask
nobs  0 99  number of vaild data
ENDVARS
