#####################

# ocsv001
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl

# ocsv011
LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

targets = nc2grads_ceres \
	nc2grads_ceres_net_ocn \
	nc2grads_ceres_reduce_seaice \
	compare_rad_buoy \
	mkmonclim \
	reduce_swdn_clim \
	filter_rad_clim \
	txt2grads_station

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

nc2grads_ceres : nc2grads_ceres.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_ceres_net_ocn : nc2grads_ceres_net_ocn.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDNCOPTS) -o $@

nc2grads_ceres_reduce_seaice : nc2grads_ceres_reduce_seaice.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDNCOPTS) -o $@

#compare_swdn_buoy : compare_swdn_buoy.o
#	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

#mkmonclim : mkmonclim.o
#	$(F90) $(FFLAGS) $^ -o $@
