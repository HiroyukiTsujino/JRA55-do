
targets := make_org_to_daily_surf \
	   make_org_to_monthly_flux \
	   make_daily_to_monthly \
	   make_daily_to_monthly_with_missing \
	   make_org_to_monthly_surf \
	   make_org_to_monthly_wind \
	   make_org_to_monthly_surf_onice

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ *.d

vpath %.F90 src src/file_mng src/util

include ../setting/macros.make

LIB_DIR := ../lib
include ../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o reduced_grid.o
$(lib) : $(objs_lib)
	$(AR) $@ $^

$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@


### Dependencies ###

make_daily: make_daily.o
