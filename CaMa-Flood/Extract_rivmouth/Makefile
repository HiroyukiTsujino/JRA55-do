FC      = ifort -convert big_endian -assume byterecl
#FC      = ifort -convert little_endian -assume byterecl
AR      = ar  -r
RANLIB  = ls
RM      = rm -f

FOPTION =
LIB     =
MODULE  =
OBJLIB  =
TARGETS = mk_runoff_v1.0_mri

all: $(MODULE) $(LIB) $(TARGETS)

$(LIB) : $(OBJLIB)
	$(AR) $(LIB) $(OBJLIB)
	$(RANLIB) $(LIB)

mk_runoff_v1.0_mri : mk_runoff_v1.0_mri.o
	$(FC) -o $@ mk_runoff_v1.0_mri.o $(FOPTION) $(LIB) 

clean  :
	$(RM) *.o *~ *.trace *.mod core $(LIB) $(TARGETS)


.SUFFIXES :
.SUFFIXES : .F .o .mod

.F.o   :
	$(FC) $< $(FOPTION) -c 

.F.mod  :
	$(FC) $< -c  
