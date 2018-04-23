### Target objects ###
targets := sverdrup_time_series

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ *.ctl *.d namelist.test

vpath %.F90 src ../file_mng

### Platform dependent settings ###
include ../../setting/macros.make

#LDOPTS = -L/usr/local/netcdf/lib -lnetcdf

### MXE library ###
LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := hintpl.o fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $<


####################

CPPFLAGS =

### Compile ###
%.o: %.F90 $(LIBMXE) $(lib)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###
hintpl.o: hintpl.F90
fileunmng.o: fileunmng.F90
fileocmng.o: fileocmng.F90 fileunmng.o
sverdrup_time_series.o: sverdrup_time_series.F90 fileocmng.o
