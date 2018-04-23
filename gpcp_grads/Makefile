#

include ../setting/macros.make

#FC = pgf90
#F90 = pgf90
#FFLAGS = -byteswapio
#
# ocsv001
#LDOPTS = -L/usr/local/netcdf/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm -lcurl
#INCLUDES = -I/usr/local/netcdf/include
#
## ocsv003 
#LDOPTS = -L/usr/local/netcdf-fortran/lib -lnetcdff -L/usr/local/netcdf/lib -lnetcdf -lm 
#INCLUDES = -I/usr/local/netcdf-fortran/include

RM = /bin/rm -f

TARGETS = nc2grads_gpcp

vpath %.F90 src

####################
#CPPFLAGS = -DOGCM_TAUBULK -DOGCM_BULKITER
#CPPFLAGS = -DOGCM_BULKITER
CPPFLAGS =
INCLUDES = $(INCOPTS)

### Compile ###
%.o: %.F90 $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(NETCDF_INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) $(NETCDF_LDFLAGS) -o $@

#####################

all : $(TARGETS)

#nc2grads_gpcp : nc2grads_gpcp.o
#	$(F90) $(FFLAGS) $^ $(LDFLAGS) $(LDOPTS) -o $@

clean:
	$(RM) *.o *.L *~ $(TARGETS) core.*
