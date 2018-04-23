### Target objects ###
targets := mk_wind_fill_divide mk_wind_fill_unite \
	mk_wind_sv_divide mk_wind_sv_unite \
	cobe_to_jra55_mask \
	smooth_weight \
	smooth_weight_ens4

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
objs_lib := fileunmng.o fileocmng.o
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
mk_wind_fill_divide: mk_wind_fill_divide.o
mk_wind_fill_divide.o: mk_wind_fill_divide.F90

mk_wind_fill_unite: mk_wind_fill_unite.o
mk_wind_fill_unite.o: mk_wind_fill_unite.F90
