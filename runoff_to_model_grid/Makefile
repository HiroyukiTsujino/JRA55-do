targets := mk_mask rivermouth_ctl

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src

### Platform dependent settings ###
F90     = gfortran
FFLAGS  = -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace -fconvert=big-endian
LDFLAGS =
AR      = ar rv
INCLUDES=

%.o: %.F90 
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $< 

## Combine ###
%: %.o $(lib) 
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@ -C
#%: %.o $(lib) 
#	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@ 


mk_mask.o: mk_mask.F90
mk_mask: mk_mask.o
rivermouth.o: rivermouth.F90 location.o distance.o
rivermouth_ctl.o: rivermouth_ctl.F90 rivermouth.o location.o distance.o
rivermouth_ctl: rivermouth.o rivermouth_ctl.o location.o distance.o
