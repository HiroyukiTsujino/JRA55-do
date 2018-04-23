### Target objects ###
targets := use_scat_daily_example \
	   qscat_rss_to_daily \
	   daily2monthly \
	   zonal_mean

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ namelist.test

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make


### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o get_scat_daily_v4.o
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
get_scat_daily_v4.o: get_scat_daily_v4.F90
use_scat_daily_example.o: use_scat_daily_example.F90
qscat_rss_to_daily.o: qscat_rss_to_daily.F90
daily_to_monthly.o: daily_to_monthly.F90
zonal_mean.o: zonal_mean.F90
