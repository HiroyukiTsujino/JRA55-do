# -*-Makefile-*-

### Target objects ###
targets := remap_vector


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src
vpath %.F src

### Platform dependent settings ###
include ../../setting/macros.make

### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local modules ###
#lib := lib.a
#objs_lib :=
#$(lib) : $(objs_lib)
#	$(AR) $@ $^
#$(objs_lib): %.o: %.F90 $(LIBMXE)
#	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Compile ###
%.o: %.F90 $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

gauss.o : gauss.F
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o
	$(F90) $^ $(LDFLAGS) -o $@

### Dependencies ###
remap_vector: remap_vector.o structure_mxe.o param_mxe.o gaussgrid.o gauss.o
remap_vector.o: remap_vector.F90 structure_mxe.o param_mxe.o gaussgrid.o gauss.o
gaussgrid.o: gaussgrid.F90 gauss.o

### Wrapper of ANL ###
include ../wrapper_ANL/mkdep
