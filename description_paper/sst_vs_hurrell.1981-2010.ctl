dset ^../linkdir/COBESST/clim/sst_vs_hurrell.1981-2010.gd
undef -9.99E+33
title COBE-SST monthly
options big_endian
ydef 180 linear -89.500000 1.000000
xdef 360 linear 0.500000 1.000000
# 2014/02/18:  T=1729 Jan2014
tdef 1 linear jan1958 1mo
zdef 1 linear 1 1
vars 2
diff  0 80 ,160,0  ** ssT [C.Deg.]
rmsd  0 80 ,160,0  ** ssT [C.Deg.]
ENDVARS
