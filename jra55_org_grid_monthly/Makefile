
targets := blend_iabp_npoles_anl \
	   make_monclim \
	   make_monclim_on_ice \
	   make_monclim_extrap

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~


vpath %.F90 src src/file_mng src/util

include ../setting/macros.make

LIB_DIR := ../lib
include ../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o reduced_grid.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o $(lib) $(LIBMXE)
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###

blend_iabp_noples_anl: blend_iabp_noples_anl.o
make_monclim: make_monclim.o

make_monclim_extrap: make_monclim_extrap.o

make_monclim_on_ice: make_monclim_on_ice.o
