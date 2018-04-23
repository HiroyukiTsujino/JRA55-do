### Target objects ###
targets := calib_tmp2m_interannual \
	   calib_sph2m_interannual \
	   calib_precip_interannual \
	   calib_rad_interannual \
	   filter_wind_interannual \
	   calib_wind_interannual \
	   calib_windmag_interannual \
	   calib_wind_offset_interannual \
	   calib_wind_anom_mag_interannual \
	   precip_minus_snow_interannual \
	   calib_tmp2m_ocn_interannual \
	   calib_tmp2m_ice_interannual \
	   calib_tmp2m_ocn_ice_interannual \
	   calib_sph2m_ocn_interannual

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src src/file_mng src/util

### Platform dependent settings ###
include ../setting/macros.make

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o force_process.o reduced_grid.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $<


####################

### Compile ###
%.o: %.F90 $(LIBMXE) $(lib)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
fileunmng.o: fileunmng.F90
fileocmng.o: fileocmng.F90 fileunmng.o
reduced_grid.o: reduced_grid.F90
