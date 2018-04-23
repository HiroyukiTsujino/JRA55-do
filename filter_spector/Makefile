# -*-Makefile-*-
#
FC = pgf90
AR = ar -r
RM = rm -f

FOPTION = -lacml -lacml_mv -byteswapio -fastsse -Mvect=sse -Mcache_align -Minfo -mcmodel=medium -tp k8-64
LINKOPT = 
LIB     = 
LIB_NM  = 
LIB_LG  = 
OBJLIB  = 
OBJLIB_NM  = 
OBJLIB_LG = 
TARGETS = wave_number_fft wave_number_frequency_fft make_weight

all: $(MODULE) $(LIB) $(LIB_LG) $(TARGETS)

clean:
	$(RM) *.o *.a *.trace *~ *.mod *.M core $(LIB)
#

vpath %.F90 src src/file_mng

### Local modules ###
lib := lib.a
objs_lib := fileunmng.o fileocmng.o
$(lib) : $(objs_lib)
	$(AR) $@ $^
$(objs_lib): %.o: %.F90
	$(FC) -c $(FOPTION) $(INCLUDES) -o $@ $<

### Compile ###

%.o: %.F90 $(lib) $(LIBMXE)
	$(FC) -c $(FOPTION) $(INCLUDES) -o $@ $<

### Link ###
%: %.o $(lib)
	$(FC) $^ $(FOPTION) -o $@
#

wave_number_fft : wave_number_fft.o
	$(FC) -o $@ wave_number_fft.o $(FOPTION) 

wave_number_frequency_fft : wave_number_frequency_fft.o
	$(FC) -o $@ wave_number_frequency_fft.o $(FOPTION) 

wave_number_frequency_fit : wave_number_frequency_fit.o
	$(FC) -o $@ wave_number_frequency_fit.o $(FOPTION) 
