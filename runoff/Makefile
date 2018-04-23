### Target objects ###
targets := total_runoff check_nextxy apply_factor apply_factor_noyrev \
	merge_cama_core \
	create_final_product \
	runoff_integ \
	total_runoff_monthly \
	total_runoff_monclim \
	total_runoff_annclim \
	dummy_topo \
	core_to_cama \
	create_1x1_monthly \
	runoff_antarctica_region

### Phony targets ###
all: $(targets)
clean:
	rm -f *.o $(targets) *_driver *.mod *.a *~ */*~

vpath %.F90 src src/file_mng

### Platform dependent settings ###
include ../setting/macros.make

### MXE library ###
LIB_DIR := ../lib
include ../lib/libmxe.make

### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o
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
