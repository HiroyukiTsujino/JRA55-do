
targets_test := snap_driver mean_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

snap_driver: snap_driver.o snap_test.o snap.o
snap_driver.o: snap_driver.F90 snap_test.o
snap_test.o: snap_test.F90 snap.o

mean_driver: mean_driver.o mean_test.o mean.o
mean_driver.o: mean_driver.F90 mean_test.o
mean_test.o: mean_test.F90 mean.o

