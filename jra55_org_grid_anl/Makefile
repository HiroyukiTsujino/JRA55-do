
### Target objects ###
targets := blend_quikscat_anl \
	   blend_quikscat_anl_neutral \
	   blend_remss_wind_anl_neutral \
	   blend_iabp_npoles_anl \
	   quikscat_on_jra55_grid \
	   remss_wind_on_jra55_grid \
	   erai_on_jra55_grid \
	   erai_with_miss_on_jra55_grid \
	   era40_on_jra55_grid \
	   reanl_on_jra55_grid \
	   zonal_mean_on_jra55_grid \
	   diag_neutral_wind \
	   diag_actual_wind \
	   jra55reg_to_jra55red \
	   iabp_npoles_on_jra55_grid \
	   make_ensemble_mean \
	   make_ensemble_mean_weight

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~

vpath %.F90 src src/file_mng src/util

include ../setting/macros.make

LIB_DIR := ../lib
include ../lib/libmxe.make

### OpenMP ###

FFLAGS  := $(FFLAGS) -mp
LDFLAGS := $(LDFLAGS) -mp

### Local library ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o reduced_grid.o distance_rad.o sort.o
$(lib) : $(objs_lib)
	$(AR) $@ $^

$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###

quikscat_on_jra55_grid: quikscat_on_jra55_grid.o shift_wind_neutral_to_real.o
blend_quikscat_anl: blend_quikscat_anl.o shift_wind_neutral_to_real.o
blend_iabp_noples_anl: blend_iabp_noples_anl.o
remss_wind_on_jra55_grid: remss_wind_on_jra55_grid.o
diag_neutral_wind: diag_neutral_wind.o shift_wind_real_to_neutral.o
diag_actual_wind: diag_actual_wind.o shift_wind_neutral_to_real.o
