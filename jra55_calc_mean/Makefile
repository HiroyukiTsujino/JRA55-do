FC = pgf90
F90 = pgf90
FFLAGS = -byteswapio
# ocsv001
LDOPTS = -lm -lcurl

RM = /bin/rm -f

TARGETS = \
	make_org_to_monthly \
	make_org_to_monthly_scalar \
	make_org_to_daily \
	make_org_to_monthly_wind \
	make_org_to_monthly_max_min

vpath %.F90 src

%.o: %.F90
	$(F90) $(CPPFLAGS) $(FFLAGS) $(LDOPTS) -c $< -o $@

all : $(TARGETS)

make_org_to_daily : make_org_to_daily.o
	$(FC) $(FFLAGS) $^ -o $@

make_org_to_monthly : make_org_to_monthly.o
	$(FC) $(FFLAGS) $^ -o $@

make_org_to_monthly_scalar : make_org_to_monthly_scalar.o
	$(FC) $(FFLAGS) $^ -o $@

make_org_to_monthly_max_min : make_org_to_monthly_max_min.o
	$(FC) $(FFLAGS) $^ -o $@

make_org_to_monthly_wind : make_org_to_monthly_wind.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	$(RM) *.o *.L *~ $(TARGETS) core.*
