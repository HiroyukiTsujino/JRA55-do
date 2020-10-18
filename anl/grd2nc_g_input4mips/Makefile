### Target objects ###
targets := grd2nc_g grd2nc_g_all

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src src/wrapper_ANL

### Platform dependent settings ###
include ../../setting/macros.make

### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make

### Local modules ###
lib := lib.a
objs_lib := param_mxe.o structure_mxe.o netcdf_general.o var_4d_wr_3d.o var_3d_wr_2d.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(NETCDF_FFLAGS) $(INCLUDES) $(INCOPTS) $(NETCDF_INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(INCOPTS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) $(LDOPTS) $(NETCDF_LDFLAGS) -o $@

### Dependencies ###

#grd2nc_g.o: grd2nc_g.F90 var_4d_wr_3d.o var_3d_wr_2d.o

### Wrapper of ANL ###
include ../wrapper_ANL/mkdep
