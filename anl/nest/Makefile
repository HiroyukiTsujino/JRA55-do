
targets := remap_ctl

all: $(targets)
clean:
	rm -f *.o core $(targets) *.mod *.a *~ */*~ *.ctl \
	 *_driver log.* namelist.test

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


remap_ctl  : remap_ctl.o   remap.o
remap_ctl.o: remap_ctl.F90 remap.o


include test/test.make
