
targets := produce_tmp_sph_10m \
	   make_mapping_table \
	   make_red2reg \
	   make_red2reg_with_miss \
	   make_red2reg_tmp2m_corrfac \
	   make_red2reg_multiply_corrfac

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~

vpath %.F90 src src/util src/file_mng

include ../setting/macros.make
# use OpenMP with pgf90
#F90     = pgf90
# OpenMP
#FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio -mp
#LDFLAGS = -mcmodel=medium -lacml -lacml_mv -mp
# no OpenMP
#FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -lacml_mv -O -byteswapio
#LDFLAGS = -mcmodel=medium -lacml -lacml_mv
AR      = ar rv
INCLUDES=

LIB_DIR := ../lib
include ../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o reduced_grid.o distance_rad.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o $(lib) $(LIBMXE)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###

produce_tmp_sph_10m: produce_tmp_sph_10m.o shift_wind_real_to_neutral.o
