
### Target objects ###
targets := snap_ctl mean_ctl


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
snap_ctl: snap_ctl.o snap.o
snap_ctl.o: snap_ctl.F90 snap.o

mean_ctl: mean_ctl.o mean.o
mean_ctl.o: mean_ctl.F90 mean.o


### Unit Test ###
include test/test.make
