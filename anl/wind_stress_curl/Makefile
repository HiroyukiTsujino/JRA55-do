### Target objects ###
targets := curltau


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src ../wrapper_ANL ../file_mng

### Platform dependent settings ###
include ../../setting/macros.make


### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := param_mxe.o structure_mxe.o fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###
curl_tau: curl_tau.o

### Wrapper of ANL ###
include ../wrapper_ANL/mkdep
