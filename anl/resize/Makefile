
targets := trim_hs_ctl

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ *.d *.ctl \
	 namelist.test

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


%.o: %.F90 $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
trim_hs_ctl  : trim_hs_ctl.o   trim_hs.o
trim_hs_ctl.o: trim_hs_ctl.F90 trim_hs.o


include test/test.make
