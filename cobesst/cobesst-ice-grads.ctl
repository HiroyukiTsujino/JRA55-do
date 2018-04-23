dset ^ice-glb.%y4%m2
undef -9.99e33
title COBE-SST monthly
options template big_endian
ydef 180 linear -89.500000 1.000000
xdef 360 linear 0.500000 1.000000
# 2014/02/18:  T=1729 Jan2014
tdef 1729 linear jan1870 1mo
zdef 1 linear 1 1
vars 1
i  0 91 ,160,0  ** Ice concentration (ice=1;no ice=0) [fraction]
ENDVARS
