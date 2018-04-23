
### Target objects ###
targets = mk_correc_tmp2m \
	  mk_correc_tmp2m_fill_with_all \
	  mk_correc_sph2m \
	  mk_correc_rad   \
	  mk_correc_rad_on_native \
	  mk_correc_precip \
	  mk_correc_wvec_ceof_mon \
	  mk_correc_wvec_tanh_mon \
	  mk_correc_wvec_ceof_ann \
	  mk_correc_wvec_tanh_ann \
	  mk_correc_wmag_ann \
	  mk_correc_wmag_offset_ann

### Phony targets ###
all: $(targets)
clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src src/file_mng src/util

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
objs_lib := fileunmng.o fileocmng.o reduced_grid.o
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

mk_correc_tmp2m: mk_correc_tmp2m.o
