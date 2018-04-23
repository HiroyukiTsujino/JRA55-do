### Target objects ###
targets := grd2nc_g


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src src/wrapper_ANL

### Platform dependent settings ###
include ../../setting/macros.make

# ocsv011
LDOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm
INCOPTS = -I/usr/local/netcdf/include


### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := param_mxe.o structure_mxe.o var_4d_wr_3d.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) $(INCOPTS) -o $@ $<


### Compile ###
%.o: %.F90 $(lib)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(INCOPTS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) $(LDOPTS) -o $@

### Dependencies ###

grd2nc_g.o: grd2nc_g.F90

### Wrapper of ANL ###
include ../wrapper_ANL/mkdep
