
targets := remap_ctl interpolate_ctl

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ *.d *.ctl \
	 namelist.test

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := force_data.o weight.o coastline.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
remap_ctl:   remap_ctl.o   remap.o
remap_ctl.o: remap_ctl.F90 remap.o

interpolate_ctl:   interpolate_ctl.o   interpolate.o
interpolate_ctl.o: interpolate_ctl.F90 interpolate.o


include test/test.make
