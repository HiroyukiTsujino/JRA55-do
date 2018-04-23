#F90 = pgf90
#FFLAGS = -byteswapio

targets := nc2grads_ciaf_runoff integ_ciaf_runoff \
	divide unite mkmonclim_mask mkannclim_mask \
	remap_on_cobesst_grid

all: $(targets)
clean:
	rm -f *.o *.L *~ $(targets) core.*

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make

#LDFLAGS += -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
LDFLAGS += -L/usr/local/netcdf/lib -lnetcdf -lm

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
fileunmng.o: fileunmng.F90
fileocmng.o: fileocmng.F90 fileunmng.o
