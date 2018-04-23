#####################

LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl

targets = nc2grads_srb nc2grads_srb3_0lp_net_ocn nc2grads_srb3_1_net_ocn little2big_srb merge_core_srb mkmonclim

vpath %.F90 src

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

nc2grads_srb : nc2grads_srb.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

nc2grads_srb_net_ocn : nc2grads_srb_net_ocn.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDNCOPTS) -o $@

nc2grads_srb3_0lp_net_ocn : nc2grads_srb3_0lp_net_ocn.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDNCOPTS) -o $@

nc2grads_srb3_1_net_ocn : nc2grads_srb3_1_net_ocn.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDNCOPTS) -o $@

little2big_srb : little2big_srb.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

mkmonclim : mkmonclim.o
	$(F90) $(FFLAGS) $^ -o $@
