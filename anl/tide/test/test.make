
targets_test := energy_driver regrid_driver gradient_driver \
  uabs_mean_driver jcoast_driver map_driver cap_jcoast_driver \
  btro_vector_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

btro_vector_driver  : btro_vector_driver.o   btro_vector_test.o btro_vector.o
btro_vector_driver.o: btro_vector_driver.F90 btro_vector_test.o
btro_vector_test.o  : btro_vector_test.F90   btro_vector.o

regrid_driver: regrid_driver.o regrid_test.o regrid.o
regrid_driver.o: regrid_driver.F90 regrid_test.o
regrid_test.o: regrid_test.F90 regrid.o

map_driver: map_driver.o map_test.o map.o
map_driver.o: map_driver.F90 map_test.o
map_test.o: map_test.F90 map.o

gradient_driver: gradient_driver.o gradient_test.o gradient.o
gradient_driver.o: gradient_driver.F90 gradient_test.o
gradient_test.o: gradient_test.F90 gradient.o

jcoast_driver: jcoast_driver.o jcoast_test.o jcoast.o
jcoast_driver.o: jcoast_driver.F90 jcoast_test.o
jcoast_test.o: jcoast_test.F90 jcoast.o

cap_jcoast_driver: cap_jcoast_driver.o cap_jcoast_test.o cap_jcoast.o
cap_jcoast_driver.o: cap_jcoast_driver.F90 cap_jcoast_test.o
cap_jcoast_test.o: cap_jcoast_test.F90 cap_jcoast.o

energy_driver: energy_driver.o energy_test.o energy.o
energy_driver.o: energy_driver.F90 energy_test.o
energy_test.o: energy_test.F90 energy.o

uabs_mean_driver: uabs_mean_driver.o uabs_mean_test.o uabs_mean.o
uabs_mean_driver.o: uabs_mean_driver.F90 uabs_mean_test.o
uabs_mean_test.o: uabs_mean_test.F90 uabs_mean.o
