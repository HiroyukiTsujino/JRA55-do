###
### Wavelet library ( Fortran modules )

include ../macros.make

LIBWAVELET := libwavelet.a
objs_libwavelet :=  chisqr.o cfftpack.o wavelet.o

$(LIBWAVELET) : $(objs_libwavelet)
	$(AR) $@ $?


# pgf90 works!

%.o: %.f
	$(F90) $< -c $(FFLAGS) -o $@

cfftpack.o: cfftpack.f

chisqr.o: chisqr.f

wavelet.o: wavelet.f cfftpack.o chisqr.o

clean:
	rm -f *.o core *.mod *.a *~ */*~ 2> /dev/null
