### Target objects ###
targets := gmmoc_depth


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src

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

### Link ###
%: %.o
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
gmmoc_depth: gmmoc_depth.o structure_mxe.o param_mxe.o
gmmoc_depth.o: gmmoc_depth.F90 structure_mxe.o param_mxe.o

### Wrapper of ANL ###
include ../wrapper_ANL/mkdep
