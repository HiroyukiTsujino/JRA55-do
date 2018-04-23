FC = pgf90
F90 = pgf90
FFLAGS = -byteswapio

# ocsv001
#LDOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
#INCLUDES = -I/usr/local/netcdf/include

# ocsv003 
LDOPTS = -L/usr/local/netcdf-fortran/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm 
INCLUDES = -I/usr/local/netcdf-fortran/include

RM = /bin/rm -f

TARGETS = nc2grads_ncep1 \
	nc2grads_ncep1_const \
	diag_bulk \
	mkmonclim mkmonclim_on_ice \
	mkanncl_from_moncl

vpath %.F90 src

####################

#CPPFLAGS = -DOGCM_TAUBULK -DOGCM_BULKITER
CPPFLAGS = -DOGCM_BULKITER

### Compile ###
%.o: %.F90 $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

#####################

all : $(TARGETS)

nc2grads_ncep1 : nc2grads_ncep1.o
	$(FC) $(FFLAGS) $^ $(LDOPTS) -o $@

nc2grads_ncep1_const : nc2grads_ncep1_const.o
	$(FC) $(FFLAGS) $^ $(LDOPTS) -o $@

mkmonclim : mkmonclim.o
	$(FC) $(FFLAGS) $^ -o $@

mkmonclim_on_ice : mkmonclim_on_ice.o
	$(FC) $(FFLAGS) $^ -o $@

mkanncl_from_moncl : mkanncl_from_moncl.o
	$(FC) $(FFLAGS) $^ -o $@

diag_bulk : diag_bulk.o bulk-ncar.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	$(RM) *.o *.L *~ $(TARGETS) core.*
