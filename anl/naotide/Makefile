
### Target objects ###
targets := tidehmap_ctl tidehmap_ctl_j tidehmap_ctl_M2 \
 tidehmap_ctl_K1 tidehmap_ctl_j_M2 spot_ctl spot_ctl_j


### Phony targets ###
all: $(targets)
clean:
	@rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl namelist.test


### Platform dependent settings ###
include ../../setting/macros.make


### Library settings ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Compile ###
%.o: %.F90 $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%.o: %.f
	$(F90) $< -c $(FFLAGS) -o $@


### Link ###
%: %.o
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
tidehmap_ctl: tidehmap_ctl.o tidehmap.o naotide.o
tidehmap_ctl.o: tidehmap_ctl.F90 tidehmap.o

tidehmap_ctl_j: tidehmap_ctl.o tidehmap.o naotidej.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@

tidehmap_ctl_M2: tidehmap_ctl.o tidehmap.o naotide_M2.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@

tidehmap_ctl_K1: tidehmap_ctl.o tidehmap.o naotide_K1.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@

tidehmap_ctl_j_M2: tidehmap_ctl.o tidehmap.o naotidej_M2.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@

spot_ctl: spot_ctl.o spot.o naotide.o
spot_ctl.o: spot_ctl.F90 spot.o

spot_ctl_j: spot_ctl.o spot.o naotidej.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@


### Unit Test ###
include test/test.make
