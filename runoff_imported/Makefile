### Target objects ###
targets := transfer_endian delete_lake find_waterhead \
           remove_Greenland_Antarctica \
           remove_Greenland_Antarctica_update \
           mk_AntFreshFlux_Depoorter \
           mk_GrnFreshFlux_Bamber \
           mk_GrnFreshFlux_Bamber_2018 \
           mk_GrnFreshFlux_CORE \
           mk_GrnCaaFreshFlux_Bamber_2018 \
           read_bamber_fwf \
           scan_noyrev \
           divide unite

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *.mod *.a *~ *.d *.gd *.ctl 2> /dev/null

vpath %.F90 src

### Platform dependent settings ###
include ../setting/macros.make

%.o: %.F90 
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $< 

## Combine ###
#%: %.o
#	$(F90) $^ $(FFLAGS) $(LDFLAGS) $(lib) -o $@ -C
%: %.o $(lib) 
	$(F90) $^ $(FFLAGS) $(LDFLAGS) $(lib) -o $@ 


transfer_endian.o: transfer_endian.F90
transfer_endian:   transfer_endian.o
delete_lake.o: delete_lake.F90
delete_lake:   delete_lake.o
find_waterhead.o: waterhead.o find_waterhead.F90
find_waterhead:   waterhead.o find_waterhead.o
mk_AntFreshFlux_Depoorter.o: mk_AntFreshFlux_Depoorter.F90
mk_AntFreshFlux_Depoorter  : mk_AntFreshFlux_Depoorter.o
mk_GrnFreshFlux_Bamber.o: mk_GrnFreshFlux_Bamber.F90
mk_GrnFreshFlux_Bamber  : mk_GrnFreshFlux_Bamber.o
mk_GrnFreshFlux_CORE.o: mk_GrnFreshFlux_CORE.F90
mk_GrnFreshFlux_CORE  : mk_GrnFreshFlux_CORE.o
mk_GrnFreshFlux_Bamber_2018.o: mk_GrnFreshFlux_Bamber_2018.F90
mk_GrnFreshFlux_Bamber_2018  : mk_GrnFreshFlux_Bamber_2018.o
mk_GrnCaaFreshFlux_Bamber_2018.o: mk_GrnCaaFreshFlux_Bamber_2018.F90
mk_GrnCaaFreshFlux_Bamber_2018  : mk_GrnCaaFreshFlux_Bamber_2018.o
remove_Greenland_Antarctica.o: remove_Greenland_Antarctica.F90 
remove_Greenland_Antarctica:   remove_Greenland_Antarctica.o
read_bamber_fwf.o: read_bamber_fwf.F90
read_bamber_fwf  : read_bamber_fwf.o
scan_noyrev.o: scan_noyrev.F90
scan_noyrev  : scan_noyrev.o
divide.o: divide.F90
divide  : divide.o
