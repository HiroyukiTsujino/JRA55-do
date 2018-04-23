FC = pgf90
F90 = pgf90
FFLAGS = -byteswapio
# ocsv004
#LDOPTS = -L/usr/local/netcdf/lib -lnetcdf
# ocsv001
#LDOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
# ocsv011
LDOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

RM = /bin/rm -f

TARGETS = nc2grads_npoles nc2grads_npoles_clim mkmonclim mkmonclim_on_ice

vpath %.F90 src

all: $(TARGETS)

.SUFFIXES: .F90

.F90.o:
	$(F90) $(CPPFLAGS) $(FFLAGS) $(LDOPTS) -c $< -o $@

all : $(OBJS)

nc2grads_npoles : nc2grads_npoles.o
	$(FC) $(FFLAGS) $^ $(LDOPTS) -o $@

nc2grads_npoles_clim : nc2grads_npoles_clim.o
	$(FC) $(FFLAGS) $^ $(LDOPTS) -o $@

mkmonclim_on_ice : mkmonclim_on_ice.o
	$(FC) $(FFLAGS) $^ -o $@

mkmonclim : mkmonclim.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	$(RM) *.o *.L *~ $(TARGETS) core.*
