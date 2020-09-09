### Target objects ###
targets := diag_surf_interannual_on_sst diag_bulk_interannual_on_sst \
	   diag_rad_interannual_on_sst diag_precip_interannual_on_sst \
	   diag_evap_interannual_on_sst \
	   diag_netrad_interannual_on_sst \
	   diag_hturb_interannual_on_sst \
	   diag_bulk_interannual_on_sst_wind \
	   shift_surf_interannual_ocean

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../../setting/macros.make

### ocsv001 / ocsv003 / ocsv005 / ocsv011 ###
#F90     = pgf90
# OpenMP
#FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio -mp
#LDFLAGS = -mcmodel=medium -lacml -lacml_mv -mp
# no OpenMP
#FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio
#LDFLAGS = -mcmodel=medium -lacml -lacml_mv

#
AR      = ar rv
INCLUDES=

#LDOPTS = -L/usr/local/netcdf/lib -lnetcdf

### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make

### OpenMP ###

FFLAGS  := $(FFLAGS) $(OMPFFLAGS)
LDFLAGS := $(LDFLAGS) $(OMPLDFLAGS)

### Local modules ###
lib := lib.a
objs_lib := hintpl.o bulk-ncar.o bulk-ncar-shift.o \
	fileunmng.o fileocmng.o \
	force_process.o mapping_table.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $<


####################

#CPPFLAGS = -DOGCM_TAUBULK -DOGCM_BULKITER -DOGCM_LYCOEF
CPPFLAGS = -DOGCM_TAUBULK -DOGCM_BULKITER

### Compile ###
%.o: %.F90 $(LIBMXE) $(lib)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
bulk-ncar.o: bulk-ncar.F90
bulk-ncar-shift.o: bulk-ncar-shift.F90
hintpl.o: hintpl.F90
fileunmng.o: fileunmng.F90
fileocmng.o: fileocmng.F90 fileunmng.o
mapping_table.o: mapping_table.F90
force_process.o: force_process.F90
diag_evap_interannual_on_sst.o: diag_evap_interannual_on_sst.F90 fileocmng.o force_process.o
diag_bulk_interannual_on_sst.o: diag_bulk_interannual_on_sst.F90
diag_bulk_interannual_on_sst_wind.o: diag_bulk_interannual_on_sst_wind.F90
diag_rad_interannual_on_sst.o: diag_rad_interannual_on_sst.F90
diag_precip_interannual_on_sst.o: diag_precip_interannual_on_sst.F90
diag_surf_interannual_on_sst.o: diag_surf_interannual_on_sst.F90
shift_surf_interannual_ocean.o: shift_surf_interannual_ocean.F90
