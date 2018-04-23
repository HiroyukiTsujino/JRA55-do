### Target objects ###
targets := calib_rad_interannual calib_rad_precip_interannual \
	calib_tmp2m_interannual calib_tmp10m_interannual \
	calib_tmp10m_antarc_interannual \
	calib_tmp10m_iceedge_interannual \
	calib_tmp10m_iceedge_mon_interannual \
	calib_tmp10m_iceedge_had_interannual \
	calib_tq10m_add_anom_interannual \
	calib_wind_interannual calib_sph2m_interannual  \
	calib_sph10m_interannual \
	calib_precip_interannual \
	calib_precip_mediterranean \
	precip_minus_snow_interannual \
	calc_const_calib_rad \
	calc_const_calib_prcp \
	filter_wind_interannual

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ namelist.test

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o force_process.o hintpl.o
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
