dset ^core/runoff_greenland_annual.%y4
undef -9.99E+33
title JRA55 diagnosed flux monthly
options template big_endian
xdef   1 levels 0.0
ydef   1 levels 0.0
zdef   1 linear 1 1
tdef  60 linear jan1948 1yr
vars 6
all  0 99 global run off
ao   0 99 run off from Greenland "ao"
bb   0 99 run off from Greenland "bb"
ls   0 99 run off from Greenland "ls"
is   0 99 run off from Greenland "is"
ns   0 99 run off from Greenland "ns"
ENDVARS
