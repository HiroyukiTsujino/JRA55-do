### Target objects ###

include macros.make

targets := cor3_0af cor3_0ah


### Phony targets ###
all: $(targets)
clean:
	rm -f *.o core $(targets) *_driver log.* \
	 *.mod *.a *~ */*~

vpath %.F90 src

### library ###
LIB_DIR :=

INCLUDES=
LDFLAGS=


### Local modules ###

lib := lib.a
objs_lib := psit_30.o psiu_30.o qsat.o qsee.o grv.o nrtype.o nrutil.o nr.o julday.o cor30a.o 
$(lib) : $(objs_lib)
	$(AR) $@ $^

$(objs_lib): %.o: %.F90
	$(F90) -c $(FFLAGS) $(CPPFLAGS) $(INCLUDES) -o $@ $<

### Compile ###
%.o: %.F90 $(lib) 
	$(F90) $< -c $(FFLAGS) $(INCLUDES) -o $@

### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

### Dependencies ###

cor3_0af.o: cor3_0af.F90
cor3_0ah.o: cor3_0ah.F90
