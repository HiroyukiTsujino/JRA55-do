
### Target objects ###
targets = mk_correc_wvec \
	mk_correc_wvec_mon \
	mk_correc_wvec_tanh_mon \
	mk_correc_windmag \
	mk_correc_windmag_mon \
	mk_correc_rad_mon \
	mk_correc_precip_mon \
	mk_correc_tmp10m_core \
	mk_correc_tmp10m_core_no_antarc \
	mk_correc_sph10m_core \
	calc_wind_ceof  \
	calc_wind_ceof_mon  \
	fill_wind_correc \
	fill_wind_correc_antarc \
	fill_wind_correc_antarc_v2 \
	smooth_wind_mag \
	smooth_wind_rot \
	smooth_tmp_core \
	smooth_sph_core \
	smooth_tmp_corr_tropics \
	smooth_tmp_anom_icefree_ocean \
	remove_ripples_tmp10m \
	fill_tmp_correc \
	fill_multiply_correc

### Phony targets ###
all: $(targets)
clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make


### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make

### Wavelet library ###

LIB_DIR_WAVELET := ../wavelet/lib_wavelet

LIBWAVELET := libwavelet.a

vpath $(LIB_DIR_WAVELET)

INCLUDES += -I $(LIB_DIR_WAVELET)
LDFLAGS  += -L $(LIB_DIR_WAVELET) -lwavelet

$(LIBWAVELET):
	cd $(LIB_DIR_WAVELET); make

### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o hintpl.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

### Dependencies ###
calc_wind_ceof.o: calc_wind_ceof.F90
calc_wind_ceof_mon.o: calc_wind_ceof_mon.F90
