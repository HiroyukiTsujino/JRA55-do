
### Target objects ###
targets := annclim_ctl


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


### Local library ###
lib := lib.a
objs_lib := annclim.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
annclim_ctl: annclim_ctl.o annclim.o
annnclim_ctl.o: annclim_ctl.F90 annclim.o


### Unit Test ###
#include test/test.make
