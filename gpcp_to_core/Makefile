### Target objects ###
targets := gpcp_to_core mkmonclim mk_correc_precip_mon calib_gpcp_on_core

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
gpcp_to_core: gpcp_to_core.o
gpcp_to_core.o: gpcp_to_core.F90
mkmonclim: mkmonclim.o
mkmonclim.o: mkmonclim.F90
mk_correc_precip_mon: mk_correc_precip_mon.o
mk_correc_precip_mon.o: mk_correc_precip_mon.F90
calib_gpcp_on_core: calib_gpcp_on_core.o
calib_gpcp_on_core.o: calib_gpcp_on_core.F90
