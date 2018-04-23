
targets := linear_trend

all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~ test/*~ *.d *.ctl \
	 namelist.test

vpath %.F90 src src/file_mng

include ../../setting/macros.make

LIB_DIR := ../../lib
include ../../lib/libmxe.make


### Local library ###
lib := lib.a
objs_lib := lsr.o fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^ 2>&1
$(objs_lib): %.o: %.F90 $(LIBMXE)
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $<


%.o: %.F90 $(lib) $(LIBMXE)
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

%: %.o
	$(F90) $^ $(lib) $(LDFLAGS) -o $@


### Dependencies ###

linear_trend  : linear_trend.o
linear_trend.o: linear_trend.F90
