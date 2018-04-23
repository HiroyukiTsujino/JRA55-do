FC      = ifort -convert big_endian -assume byterecl
#FC      = ifort -convert little_endian -assume byterecl
AR      = ar  -r
RANLIB  = ls
RM      = rm -f

FOPTION =
LIB     = lib.o
MODULE  = kindmodule.mod
OBJLIB  = kindmodule.o dimmodule.o gauss.o mklatm.o
TARGETS = conv_riv conv_mask
 
all: $(MODULE) $(LIB) $(TARGETS)

kindmodule.mod : kindmodule.o
	touch kindmodule.o

dimmodulde.mod : dimmodule.o
	touch dimmodule.o

$(LIB) : $(OBJLIB)
	$(AR) $(LIB) $(OBJLIB)
	$(RANLIB) $(LIB)

conv_riv : $(LIB) conv_riv.o 
	$(FC) -o $@ conv_riv.o $(FOPTION) $(LIB) 

conv_mask : $(LIB) conv_mask.o 
	$(FC) -o $@ conv_mask.o $(FOPTION) $(LIB) 

clean  :
	$(RM) *.o *~ *.trace *.mod core $(LIB) $(TARGETS)


.SUFFIXES :
.SUFFIXES : .F .o .mod

.F.o   :
	$(FC) $< $(FOPTION) -c 

.F.mod  :
	$(FC) $< -c  



