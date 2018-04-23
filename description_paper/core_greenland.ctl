dset ^../runoff/core/runoff_greenland_annual.%y4
undef -9.99E+33
title JRA55 diagnosed flux monthly
options template big_endian
XDEF 1 LINEAR     0.00000 1.0
YDEF 1 LINEAR   -90.00000 1.0
ZDEF 1 LEVELS 0.0
tdef  60 linear jan1948 1yr
vars 6
all  0 99 precipitation [Sv] on ocean
ao   0 99 precipitation [Sv] on ocean
bb   0 99 precipitation [Sv] on land
ls   0 99 precipitation [Sv] on land except for Antarctica
is   0 99 river discharge [Sv]
ns   0 99 mean river discharge [kg/m2/s]
ENDVARS
