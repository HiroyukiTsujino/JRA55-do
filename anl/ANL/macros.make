###
### Fortran 90 compiler
###

### SR16000 (front1/2) ###
# F90 = f90
# FC = $(F90)
# FFLAGS = $(F90FLAGS)
# PCPP = -D#

### IBM XL Fortran ###
# F90 = xlf -O3 -qarch=auto
# F90FLAGS = -qfree=f90 -qsuffix=f=f90:cpp=F90
# FC = $(F90)
# FFLAGS = -qfixed=72 -qsuffix=f=f:cpp=F -DBIG_ENDIAN#
# PCPP = -WF,-D# prefix for cpp

### Intel Fortran ###
#  F90 = ifort
#  F90FLAGS = -fpp -O3 -tpp7 -axW -w90 -w95 -convert big_endian -assume byterecl
#  FC = $(F90)
#  FFLAGS = $(F90FLAGS)
#  PCPP = -D#

### pgf90 (ocsv001) ###
 F90 = pgf90
 F90FLAGS = -lacml -lacml_mv -byteswapio -fastsse -Mvect=sse -Mcache_align -Minfo -Mlarge_arrays -mcmodel=medium -tp k8-64
# F90FLAGS = -byteswapio
 FC = $(F90)
 FFLAGS = $(F90FLAGS)
 FOPTION = $(F90FLAGS)
 PCPP = -D#
 AR = ar -r
 RM = rm -f

### NEC-fortran (front.mri-jma.go.jp) ###
# F90 = efc
# F90FLAGS = -Vaxlib
# FC = $(F90)
# FFLAGS = $(F90FLAGS)
# PCPP = -D#

###
### compiler-generated module file suffix
###
### Used to remove old module files for "make clean".
###

MOD_SUFF = mod

###
### change to reflect your system's netcdf installation:
###
###
### location of netcdf library (libnetcdf.a)
###

#NETCDF_LIB = /usr/lib
 NETCDF_LIB = /usr/local/lib

###
### location of Fortran netcdf include file (netcdf.inc)
###

#NETCDF_INC = /usr/include
 NETCDF_INC = /usr/local/include

###
### math library
###

#LIBMATH = #
LIBMATH = -lm# for Intel 

