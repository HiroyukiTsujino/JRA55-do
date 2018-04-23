
targets := cut_out

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ *.d *.ctl

vpath %.F90 src ../file_mng

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###

lib := lib.a
objs_lib := fileunmng.o fileocmng.o mkgrads_ctl.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o
	$(F90) $^ $(lib) $(FFLAGS) $(LDFLAGS) -o $@


### Dependencies ###
cut_out: cut_out.o
