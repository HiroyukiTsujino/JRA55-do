# Makefile for Fortran QuikSCAT HDF readers
# csh - 10/13/98
#
# HDF libraries used to link the sample program
#HDFDIR= /usr/local/hdf
HDFDIR= /usr/local/hdf4
HDFINC = $(HDFDIR)/include

LDFLAGS =  -L$(HDFDIR)/lib -lmfhdf -ldf -ljpeg -lz -lsz
#******************************************************************
#    Uncomment next 4 lines if using SUN solaris machine
#******************************************************************
#MACHINE        =       SUN
#SYS_DEF        =       -lm -lnsl
#F77            =       f77
#FFLAGS         =       -w -e -cg92 -Nl99 -I$(HDFINC)

#******************************************************************
#    Uncomment next 4 lines if using Intel x86 Linux machine & g77
#******************************************************************
#MACHINE        =       i586
#SYS_DEF        =       -lm
#F77            =       g77
#FFLAGS         =       -ffixed-line-length-132 -O2 -I$(HDFINC)

#******************************************************************
#   Uncomment next 4 lines if using SUN OS
#******************************************************************
# MACHINE        =       SUN
# SYS_DEF        =       -lm
# F77            =       f77
# FFLAGS         =       -w -I$(HDFINC)

#******************************************************************
#   Uncomment next 3 lines if using SGI machine
#******************************************************************
#  MACHINE        =       IRIX                                
#  SYS_DEF        =       -lm
#  F77            =       f77
#  FFLAGS          =       -w -I$(HDFINC)

#******************************************************************
#  Uncomment next 6 lines if using x86 Linux machine & gfortran
#******************************************************************
  MACHINE        =       Linux
  SYS_DEF        =       -lm
  F77            =       gfortran
  F90            =       gfortran
  FFLAGS         =       -w -I$(HDFINC)
  AR             =       ar rv

### Target objects ###

targets := qscat2grads qscat2daily

##### program search path #####

vpath %.F90 src src/file_mng

### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90
	$(F90) -c $(FFLAGS) -o $@ $<

### Compile ###
%.o: %.F90 $(lib)
	$(F90) $< -c $(FFLAGS) $(LDFLAGS) -o $@

### Combine ###
%: %.o
	$(F90) $^ $(lib) $(FFLAGS) $(LDFLAGS) $(SYS_DEF) -o $@

######################

qscat2grads: qscat2grads.o
qscat2grads.o: qscat2grads.F90

qscat2daily: qscat2daily.o
qscat2daily.o: qscat2daily.F90

clean:
	rm -f *.o
