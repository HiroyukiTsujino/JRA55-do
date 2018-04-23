#####################

LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl

targets = nc2grads_oaflux \
	  nc2grads_oaflux_d2a \
	  nc2grads_oaflux_tmp2m_d2a

### Phony targets ###
all: $(targets)
clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

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
	$(F90) $^ $(LDFLAGS) -o $@


###########

nc2grads_oaflux : nc2grads_oaflux.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_oaflux_d2a : nc2grads_oaflux_d2a.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_oaflux_tmp2m_d2a : nc2grads_oaflux_tmp2m_d2a.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@
