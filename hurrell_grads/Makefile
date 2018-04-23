#####################

# ocsv001
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl

# ocsv011
LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

targets = nc2grads_hurrell \
	  scan_hurrellsst \
	  mkmonclim \
	  mk_rmsd_from_moncl

### Phony targets ###
all: $(targets)
clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make


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
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

###########

nc2grads_hurrell : nc2grads_hurrell.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

scan_hurrellsst : scan_hurrellsst.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) -o $@
