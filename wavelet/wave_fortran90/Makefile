### Target objects ###

include ../macros.make

targets := wavetest wavetest_tmp10m


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src

### MXE library ###
LIB_DIR := ../lib_wavelet

LIBWAVELET := libwavelet.a

vpath $(LIBWAVELET) $(LIB_DIR)

INCLUDES += -I $(LIB_DIR)
LDFLAGS  += -L $(LIB_DIR) -lwavelet

$(LIBWAVELET):
	cd $(LIB_DIR); make


### Local modules ###

lib := lib.a
objs_lib :=
$(lib) : $(objs_lib)
	$(AR) $@ $^

### Compile ###
%.o: %.F90 $(lib) $(LIBWAVELET)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

### Dependencies ###

wavetest.o: wavetest.F90
wavetest_tmp10m.o: wavetest_tmp10m.F90
