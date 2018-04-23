### Platform dependent settings ###

include ../setting/macros.make

### Target objects ###

targets := daily2monthly annual_mean_stdv qc_monthly mkmask_monthly

##### program search path #####

vpath %.F90 src src/file_mng

### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90
	$(F90) -c $(FFLAGS) -o $@ $<

### Compile ###
%.o: %.F90 $(lib)
	$(F90) $< -c $(FFLAGS) $(LDFLAGS) -o $@

### Link ###
%: %.o
	$(F90) $^ $(lib) $(FFLAGS) $(LDFLAGS) -o $@

######################

daily2monthly: daily2monthly.o
daily2monthly.o: daily2monthly.F90

mkmask_monthly: mkmask_monthly.o
mkmask_monthly.o: mkmask_monthly.F90

qc_monthly: qc_monthly.o
qc_monthly.o: qc_monthly.F90

annual_mean_stdv: annual_mean_stdv.o
annual_mean_stdv.o: annual_mean_stdv.F90

clean:
	rm -f *.o *~ src/*~
