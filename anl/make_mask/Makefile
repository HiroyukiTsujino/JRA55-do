
targets := mk_mask_divide mk_mask_unite mk_index_unite

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ depth.txt dz.F90 *.d *.ctl \
	 namelist.test

vpath %.F90 src

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###
#lib := lib.a
#objs_lib := integ.o regrid.o
#$(lib) : $(objs_lib)
#	$(AR) $@ $^ 2>&1
#$(objs_lib): %.o: %.F90 $(LIBMXE)
#	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
mk_mask_divide: mk_mask_divide.o
mk_mask_unite: mk_mask_unite.o
