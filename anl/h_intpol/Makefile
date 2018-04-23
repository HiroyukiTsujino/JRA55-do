### Target objects ###
targets := h_intpol

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src

### Platform dependent settings ###
include ../../setting/macros.make

### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := hintpl.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $<


####################

### Compile ###
%.o: %.F90 $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
hintpl.o: hintpl.F90
h_intpol.o: h_intpol.F90
