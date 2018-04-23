dset ^core/runoff_greenland_monthly.%y4%m2
undef -9.99E+33
title JRA55 diagnosed flux monthly
options template big_endian
XDEF 1 LINEAR     0.00000 1.0
YDEF 1 LINEAR   -90.00000 1.0
ZDEF 1 LEVELS 0.0
tdef  720 linear jan1948 1mo
vars 6
all  0 99 global run off
ao   0 99 run off from Greenland "ao"
bb   0 99 run off from Greenland "bb"
ls   0 99 run off from Greenland "ls"
is   0 99 run off from Greenland "is"
ns   0 99 run off from Greenland "ns"
ENDVARS
