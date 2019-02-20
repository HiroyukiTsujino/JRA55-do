### Target objects ###
targets := diag_bulk_interannual_on_cobesst \
	   diag_bulk_interannual_on_brtmp \
	   diag_latent_fusion_interannual_on_cobesst \
	   sverdrup

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ namelist.test

vpath %.F90 src src/file_mng src/coare-3_0

### Platform dependent settings ###
#include ../setting/macros.make

### ocsv001 / ocsv003 / ocsv005 / ocsv011 ###
F90     = pgf90
# OpenMP
FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio -mp
LDFLAGS = -mcmodel=medium -lacml -lacml_mv -mp
# no OpenMP
#FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio
#LDFLAGS = -mcmodel=medium -lacml -lacml_mv

AR      = ar rv
INCLUDES=


#LDOPTS = -L/usr/local/netcdf/lib -lnetcdf

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := bulk-ncar.o \
	bulk-cor30a.o \
	psit_30.o psiu_30.o grv.o qsee.o \
	hintpl_mask.o \
	fileunmng.o fileocmng.o \
	force_process.o
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
bulk-cor30a.o: bulk-cor30a.F90
fileunmng.o: fileunmng.F90
fileocmng.o: fileocmng.F90 fileunmng.o
force_process.o: force_process.F90
diag_bulk_interannual_on_brtmp.o: diag_bulk_interannual_on_brtmp.F90
diag_bulk_interannual_on_cobesst.o: diag_bulk_interannual_on_cobesst.F90
diag_latent_fusion_interannual_on_cobesst.o: diag_latent_fusion_interannual_on_cobesst.F90
sverdrup.o: sverdrup.F90
