
### Target objects ###
targets := ts2sigma_ctl


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test


### Platform dependent settings ###
include ../../setting/macros.make


### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := density.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90
	$(F90) -c $(FFLAGS) -o $@ $<


### Compile ###
%.o: %.F90 $(LIBMXE) $(lib)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
ts2sigma_ctl: ts2sigma_ctl.o ts2sigma.o
ts2sigma_ctl.o: ts2sigma_ctl.F90 ts2sigma.o


### Unit Test ###
include test/test.make
