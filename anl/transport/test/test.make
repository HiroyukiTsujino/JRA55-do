
targets_test := wlwl_driver section_u_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test


### Dependencies ###

wlwl_driver:   wlwl_driver.o   wlwl_test.o   wlwl.o
wlwl_driver.o: wlwl_driver.F90 wlwl_test.o
wlwl_test.o:                   wlwl_test.F90 wlwl.o

section_u_driver:   section_u_driver.o   section_u_test.o   section_u.o
section_u_driver.o: section_u_driver.F90 section_u_test.o
section_u_test.o:                        section_u_test.F90 section_u.o

