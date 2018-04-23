### Target objects ###

targets = nc2grads_tao

#####################

# ocsv001
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl

# ocsv011
LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm

### Phony targets ###
all: $(targets)
clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src

### Platform dependent settings ###
include ../setting/macros.make


### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib :=
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) $(LDNCOPTS) -o $@


###########

#nc2grads_tao : nc2grads_tao.o
#	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

#mkmonclim : mkmonclim.o
#	$(F90) $(FFLAGS) $^ -o $@
