dset ^../linkdir/forcing/jra_cobesst_annual_c2/prcp_integ_2nd.%y4
undef -9.99E+33
title JRA55 diagnosed flux monthly
options template big_endian
XDEF 1 LINEAR     0.00000 1.0
YDEF 1 LINEAR   -90.00000 1.0
ZDEF 1 LEVELS 0.0
tdef  60 linear jan1958 1yr
vars 5
prcp_ocean   0 99 precipitation [Sv] on ocean
prcp_land    0 99 precipitation [Sv] on land
prcp_land_2  0 99 precipitation [Sv] on land except for Antarctica
runoff_all   0 99 river discharge [Sv]
runoff_mean  0 99 mean river discharge [kg/m2/s]
ENDVARS
