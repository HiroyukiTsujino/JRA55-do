# -*-Makefile-*-
#
### Target objects ###
targets := mon2ann_g

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src

### Platform dependent settings ###
include ../../setting/macros.make


### Compile ###
%.o: %.F90
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o
	$(F90) $^ $(LDFLAGS) -o $@
#

mon2ann_g:  mon2ann_g.o
	$(F90) -o $@ mon2ann_g.o
