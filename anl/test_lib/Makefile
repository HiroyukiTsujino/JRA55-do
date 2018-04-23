
targets := libmxe_para_driver libmxe_topo_driver libmxe_calendar_driver libmxe_grid_driver

all: $(targets)
test: $(targets)
clean:
	rm -f *.o core $(targets) *.mod *.a *~ */*~ *.ctl log.*.txt \
	 namelist.configure.in namelist.test NAMELIST.MXE

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


%.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

%: %.o
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###

libmxe_calendar_driver: libmxe_calendar_driver.o libmxe_calendar_test.o
libmxe_calendar_driver.o: libmxe_calendar_driver.F90 libmxe_calendar_test.o

libmxe_para_driver: libmxe_para_driver.o libmxe_para_test.o
libmxe_para_driver.o: libmxe_para_driver.F90 libmxe_para_test.o

libmxe_topo_driver: libmxe_topo_driver.o libmxe_topo_test.o
libmxe_topo_driver.o: libmxe_topo_driver.F90 libmxe_topo_test.o

libmxe_grid_driver:   libmxe_grid_driver.o   libmxe_grid_test.o
libmxe_grid_driver.o: libmxe_grid_driver.F90 libmxe_grid_test.o
