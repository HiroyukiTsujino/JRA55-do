
targets := power_ctl lowpass_ctl

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
objs_lib := fft_pgi.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
power_ctl: power_ctl.o power.o
power_ctl.o: power_ctl.F90 power.o

lowpass_ctl: lowpass_ctl.o lowpass.o
lowpass_ctl.o: lowpass_ctl.F90 lowpass.o


include test/test.make
