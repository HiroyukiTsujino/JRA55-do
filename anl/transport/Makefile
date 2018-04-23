
targets := wlwl_ctl section_u_ctl

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := integ.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
wlwl_ctl:   wlwl_ctl.o   wlwl.o
wlwl_ctl.o: wlwl_ctl.F90 wlwl.o

section_u_ctl:   section_u_ctl.o   section_u.o
section_u_ctl.o: section_u_ctl.F90 section_u.o


include test/test.make
