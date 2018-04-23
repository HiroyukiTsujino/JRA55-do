
### Target objects ###
targets := comp_high rms_restart


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *.mod *.a *~ 2> /dev/null


### Platform dependent settings ###
include ../../../setting/macros.make


### MXE library ###
LIB_DIR := ../../../lib
include ../../../lib/libmxe.make


### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Combine ###
%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@
