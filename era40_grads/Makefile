#####################

#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

targets = nc2grads_era40_6h2m \
	  nc2grads_era40_6hourly \
	  nc2grads_era40_spfh_6h2m \
	  nc2grads_era40_spfh_6hourly \
	  nc2grads_era40_tmp_calib_6hourly

### Phony targets ###
all: $(targets)
clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src

### Platform dependent settings ###
include ../setting/macros.make


### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib :=
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

nc2grads_era40_6h2m : nc2grads_era40_6h2m.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_era40_6hourly : nc2grads_era40_6hourly.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_era40_spfh_6h2m : nc2grads_era40_spfh_6h2m.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_era40_spfh_6hourly : nc2grads_era40_spfh_6hourly.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_era40_tmp_calib_6hourly : nc2grads_era40_tmp_calib_6hourly.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@
