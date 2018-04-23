
targets := have_ctl mean_ctl diff_mean_ctl diff_ctl vave_ctl \
 runmean_ctl barotropic_ke_vol_ctl integ_vert_ctl \
 zave_ctl have_on_ice_ctl 

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ depth.txt dz.F90 *.d *.ctl \
	 namelist.test

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := integ.o regrid.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
have_ctl: have_ctl.o have.o
have_ctl.o: have_ctl.F90 have.o

have_on_ice_ctl: have_on_ice_ctl.o have_on_ice.o
have_on_ice_ctl.o: have_on_ice_ctl.F90 have_on_ice.o

vave_ctl: vave_ctl.o vave.o
vave_ctl.o: vave_ctl.F90 vave.o

zave_ctl: zave_ctl.o zave.o
zave_ctl.o: zave_ctl.F90 zave.o

mean_ctl: mean_ctl.o mean.o
mean_ctl.o: mean_ctl.F90 mean.o

runmean_ctl: runmean_ctl.o runmean.o
runmean_ctl.o: runmean_ctl.F90 runmean.o

diff_mean_ctl: diff_mean_ctl.o diff_mean.o
diff_mean_ctl.o: diff_mean_ctl.F90 diff_mean.o

diff_ctl: diff_ctl.o diff.o
diff_ctl.o: diff_ctl.F90 diff.o

barotropic_ke_vol_ctl  : barotropic_ke_vol_ctl.o   barotropic_ke_vol.o
barotropic_ke_vol_ctl.o: barotropic_ke_vol_ctl.F90 barotropic_ke_vol.o

integ_vert_ctl  : integ_vert_ctl.o   integ_vert.o
integ_vert_ctl.o: integ_vert_ctl.F90 integ_vert.o


include test/test.make
