
targets_test := ts2sigma_driver density_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

ts2sigma_driver  : ts2sigma_driver.o ts2sigma_test.o ts2sigma.o
ts2sigma_driver.o: ts2sigma_driver.F90 ts2sigma_test.o
ts2sigma_test.o  : ts2sigma_test.F90 ts2sigma.o

density_driver  : density_driver.o density_test.o density.o
density_driver.o: density_driver.F90 density_test.o
density_test.o  : density_test.F90 density.o
