###
### MXE library ( Fortran modules )
include ../setting/macros.make

LIBMXE := libmxe.a
objs_libmxe := libmxe_para.o libmxe_calendar.o libmxe_trnsfrm.o \
 libmxe_io.o libmxe_grid.o libmxe_topo.o libmxe_grads.o \
 libmxe_stmrgn.o libmxe_ut.o
objs_fruit := fruit.o fruit_util.o

vpath fruit% fruit/


$(LIBMXE) : $(objs_libmxe) $(objs_fruit)
	$(AR) $@ $?


%.o: %.F90
	$(F90) $< -c $(FFLAGS) -o $@

libmxe_io.o: libmxe_io.F90 libmxe_para.o libmxe_calendar.o

libmxe_grid.o: libmxe_grid.F90 libmxe_para.o libmxe_trnsfrm.o

libmxe_topo.o: libmxe_topo.F90 libmxe_para.o libmxe_grid.o

libmxe_grads.o: libmxe_grads.F90 libmxe_para.o libmxe_grid.o libmxe_io.o libmxe_calendar.o

libmxe_stmrgn.o: libmxe_stmrgn.F90 libmxe_para.o


clean:
	rm -f *.o core *.mod *.a *~ */*~ 2> /dev/null


%.o: %.f90
	$(F90) $< -c $(FFLAGS) -o $@

fruit.o: fruit.f90 fruit_util.o
