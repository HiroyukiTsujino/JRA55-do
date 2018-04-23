###
### MXE library ( Fortran modules )
#- Usage: see anl/test_lib/Makefile

LIBMXE := libmxe.a

vpath $(LIBMXE) $(LIB_DIR)
INCLUDES += -I $(LIB_DIR)
LDFLAGS  += -L $(LIB_DIR) -lmxe

$(LIBMXE):
	cd $(LIB_DIR); make
