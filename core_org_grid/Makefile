### Target objects ###
targets := nc2grads_surf_ciaf_all_oneitem \
	  diag_bulk_interannual_on_cobesst \
	  monthly_ciaf \
	  monthly_ciaf_tmpcutoff \
	  mkmonclim

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ namelist.test

vpath %.F90 src src/file_mng src/coare-3_0

### Platform dependent settings ###
include ../setting/macros.make

# ocsv001
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
# ocsv011
LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

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


nc2grads_surf_ciaf_all_oneitem : nc2grads_surf_ciaf_all_oneitem.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDNCOPTS) -o $@

diag_bulk_interannual_on_cobesst.o: diag_bulk_interannual_on_cobesst.F90
