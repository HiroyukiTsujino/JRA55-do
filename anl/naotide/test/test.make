targets_test := tidehmap_driver tidehmap_M2_driver \
 tidehmap_K1_driver spot_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

tidehmap_driver: tidehmap_driver.o tidehmap_test.o tidehmap.o naotide.o
tidehmap_driver.o: tidehmap_driver.F90 tidehmap_test.o
tidehmap_test.o: tidehmap_test.F90 tidehmap.o

tidehmap_M2_driver: tidehmap_driver.o tidehmap_test.o tidehmap.o naotide_M2.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@

tidehmap_K1_driver: tidehmap_driver.o tidehmap_test.o tidehmap.o naotide_K1.o
	$(F90) $^ $(FFLAGS) $(LDFLAGS) -o $@

spot_driver: spot_driver.o spot_test.o spot.o naotide.o
spot_driver.o: spot_driver.F90 spot_test.o
spot_test.o: spot_test.F90 spot.o
