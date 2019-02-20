#####################
# ogsv007

F90     = pgf90
FFLAGS  = -tp k8-64 -mcmodel=medium -lacml -O -byteswapio
LDFLAGS = -mcmodel=medium -lacml
#LDOPTS = -L/usr/local/netcdf-4.4.0/lib -lnetcdf -L/usr/local/netcdf-fortran-4.4.3/lib -lnetcdff -lm -lcurl
#INCOPTS = -I/usr/local/netcdf-4.4.0/include -I/usr/local/netcdf-fortran-4.4.3/include
AR      = ar rv
#INCLUDES=
NETCDF_DIR      = /usr/local/netcdf-fortran-4.4.3
NETCDF_FFLAGS   = -DMXE_NETCDF
NETCDF_INCLUDES = `$(NETCDF_DIR)/bin/nf-config --fflags`
NETCDF_LDFLAGS  = `$(NETCDF_DIR)/bin/nf-config --flibs`

LDNCOPTS = -L/usr/local/netcdf-fortran-4.4.3/lib -lnetcdff -L/usr/local/netcdf-4.4.0/lib -lnetcdf -lm -lcurl
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm
#LDNCOPTS = -L/home/htsujino/build/lib -lnetcdf -lm

RM = /bin/rm -f

targets = nc2grads_globcurrent \
	globcurrent_to_jra \
	globcurrent_to_jra_onedata \
	globcurrent_smooth \
	mkannclim

### Phony targets ###
all: $(targets)
clean:
	$(RM) *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src src/file_mng

### Platform dependent settings ###
#include ../setting/macros.make

# ocsv001
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
#INCLUDES = -I/usr/local/netcdf/include

# ocsv011
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm
#INCLUDES = -I/usr/local/netcdf/include

# ocsv003 
#LDNCOPTS = -L/usr/local/netcdf-fortran/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm 
#INCLUDES = -I/usr/local/netcdf-fortran/include

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make

### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) $(NETCDF_INCLUDES) -o $@ $<

### Link ###
#%: %.o $(lib)
#	$(F90) $^ $(LDFLAGS) -o $@

#####################

all : $(TARGETS)

nc2grads_globcurrent : nc2grads_globcurrent.o
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(NETCDF_LDFLAGS) -o $@

globcurrent_to_jra : globcurrent_to_jra.o $(lib) 
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(NETCDF_LDFLAGS) -o $@

globcurrent_to_jra_onedata : globcurrent_to_jra_onedata.o $(lib) 
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(NETCDF_LDFLAGS) -o $@

globcurrent_smooth : globcurrent_smooth.o $(lib) 
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(NETCDF_LDFLAGS) -o $@

mkannclim : mkannclim.o $(lib)
	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(NETCDF_LDFLAGS) -o $@
