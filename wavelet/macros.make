###
### Fortran 90 compiler
###  Set variables: F90, FFLAGS and AR.


### ocsv001 / ocsv003 / ocsv005 / ocsv011 ###
F90     = pgf90
F77     = pgf77
FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio -mp
LDFLAGS = -mcmodel=medium -lacml -lacml_mv -mp
#FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio
#LDFLAGS = -mcmodel=medium -lacml -lacml_mv
AR      = ar rv
INCLUDES=

#FFLAGS="-tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio -Mcache_align -Minfo -Mbounds"
#- Do not use -fastsse, -O2
