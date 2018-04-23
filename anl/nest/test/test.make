
targets_test := remap_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test


remap_driver  : remap_driver.o   remap_test.o   remap.o
remap_driver.o: remap_driver.F90 remap_test.o
remap_test.o  :                  remap_test.F90 remap.o
