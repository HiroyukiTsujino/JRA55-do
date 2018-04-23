
targets := rgrid2latlon rgrid2latlon_mask rgrid2latlon_nointpol \
	lonlat2dxdy lonlat2dxdy_up \
	lonlat2dxdy_up_exact \
	shift_2m_to_10m_zrough

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ *.d \
	 namelist.test

vpath %.F90 src

include ../setting/macros.make

LIB_DIR := ../lib
include ../lib/libmxe.make


### Local library ###
#lib := lib.a
#objs_lib := fft_pgi.o
#$(lib) : $(objs_lib)
#	$(AR) $@ $^ 2>&1
#$(objs_lib): %.o: %.F90 $(LIBMXE)
#	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###

rgrid2latlon : rgrid2latlon.o

rgrid2latlon_mask : rgrid2latlon_mask.o

rgrid2latlon_nointpol : rgrid2latlon_nointpol.o

lonlat2dxdy : lonlat2dxdy.o

lonlat2dxdy_up : lonlat2dxdy_up.o

lonlat2dxdy_up_exact : lonlat2dxdy_up_exact.o

shift_2m_to_10m_zrough : shift_2m_to_10m_zrough.o bulk_shift_zrough.o
#shift_2m_to_10m_zrough : shift_2m_to_10m_zrough.o bulk_ncar_shift_hybrid.o

shift_2m_to_10m_hybrid : shift_2m_to_10m_hybrid.o bulk_ncar_shift.o

shift_2m_to_10m : shift_2m_to_10m.o shift_with_stability.o
