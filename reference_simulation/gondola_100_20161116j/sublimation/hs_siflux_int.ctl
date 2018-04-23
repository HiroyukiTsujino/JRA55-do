DSET    ^hs_siflux_int.%y4
OPTIONS big_endian TEMPLATE 365_DAY_CALENDAR
TITLE   MRICOM OCEAN MOC DATA
UNDEF   0.0
XDEF      1  LINEAR    0.0000   1.0
YDEF      1  LINEAR  -78.0000   1.0
ZDEF      1  LEVELS    0.0
TDEF  3600 LINEAR  JAN1958 1yr
VARS     4
GLB      0   99  Globally integrated sublimation [kg/s]
SBL      0   99  Globally integrated positive sublimation [kg/s]
CND      0   99  Globally integrated negative sublimation [kg/s]
ARE      0   99  Globally integrated sea ice area [m2]
ENDVARS
