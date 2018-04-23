
RM = /bin/rm -f

targets = nc2grads_gpcc \
	  scan_gpccnobs \
	  precip_integ

### Phony targets ###
all: $(targets)
clean:
	$(RM) *.o core $(targets) *.mod *.a *~ 2> /dev/null

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make

# ocsv001
#LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
#INCLUDES = -I/usr/local/netcdf/include

# ocsv011
LDNCOPTS = -L/usr/local/netcdf/lib -lnetcdf -lm
INCLUDES = -I/usr/local/netcdf/include

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
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

#####################

all : $(TARGETS)

nc2grads_gpcc : nc2grads_gpcc.o
	$(F90) $(FFLAGS) $^ $(LDNCOPTS) -o $@

scan_gpccnobs : scan_gpccnobs.o $(lib)
	$(F90) $(FFLAGS) $^ $(LDFLAGS) -o $@

precip_integ : precip_integ.o $(lib)
	$(F90) $(FFLAGS) $^ $(LDFLAGS) -o $@
