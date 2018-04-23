
targets := energy_ctl uabs_mean_ctl cap_jcoast_ctl

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := regrid.o gradient.o jcoast.o map.o btro_vector.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
cap_jcoast_ctl: cap_jcoast_ctl.o cap_jcoast.o
cap_jcoast_ctl.o: cap_jcoast_ctl.F90 cap_jcoast.o

energy_ctl: energy_ctl.o energy.o
energy_ctl.o: energy_ctl.F90 energy.o

uabs_mean_ctl: uabs_mean_ctl.o uabs_mean.o
uabs_mean_ctl.o: uabs_mean_ctl.F90 uabs_mean.o


include test/test.make
