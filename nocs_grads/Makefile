FC = pgf90
F90 = $(FC)
FFLAGS = -byteswapio

# ocsv001
#LDOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl

# ocsv011
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

RM = /bin/rm -f

TARGETS = nc2grads_nocs nc2grads_nocs_rlh diag_bulk mkmonclim mkmonclim_on_ice mkannclim mkanncl_from_moncl

all : $(TARGETS)
clean:
	$(RM) *.o *.L *~ $(TARGETS) core.*

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make

####################

#CPPFLAGS = -DOGCM_TAUBULK -DOGCM_BULKITER
CPPFLAGS = -DOGCM_BULKITER

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

#####################

nc2grads_nocs : nc2grads_nocs.o
	$(FC) $(FFLAGS) $^ $(LDOPTS) $(LDNCOPTS) -o $@

nc2grads_nocs_rlh : nc2grads_nocs_rlh.o
	$(FC) $(FFLAGS) $^ $(LDOPTS) $(LDNCOPTS) -o $@

mkmonclim : mkmonclim.o
	$(FC) $(FFLAGS) $^ -o $@

mkannclim : mkannclim.o $(objs_lib)
	$(FC) $(FFLAGS) $^ -o $@

mkmonclim_on_ice : mkmonclim_on_ice.o
	$(FC) $(FFLAGS) $^ -o $@

mkanncl_from_moncl : mkanncl_from_moncl.o
	$(FC) $(FFLAGS) $^ -o $@

diag_bulk : diag_bulk.o bulk-ncar.o
	$(FC) $(FFLAGS) $^ -o $@

