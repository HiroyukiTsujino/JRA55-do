dset ^/work116/htsujino/COBESST/monthly/yyyymm/sst-glb.%y4%m2
undef -9.99e+33
title COBE-SST monthly
options template big_endian
ydef 180 linear -89.500000 1.000000
xdef 360 linear 0.500000 1.000000
# 2014/02/18:  T=1729 Jan2014
tdef 3000 linear jan1870 1mo
zdef 1 linear 1 1
vars 1
t  0 80 ,160,0  ** ssT [C.Deg.]
ENDVARS
