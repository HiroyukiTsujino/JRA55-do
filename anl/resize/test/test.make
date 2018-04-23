
targets_test := trim_hs_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

trim_hs_driver  : trim_hs_driver.o   trim_hs_test.o trim_hs.o
trim_hs_driver.o: trim_hs_driver.F90 trim_hs_test.o
trim_hs_test.o  : trim_hs_test.F90   trim_hs.o

