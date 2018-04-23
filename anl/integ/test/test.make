
targets_test := have_driver diff_mean_driver diff_driver \
 integ_driver mean_driver integ_vert_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

integ_driver: integ_driver.o integ_test.o integ.o
integ_driver.o: integ_driver.F90 integ_test.o
integ_test.o: integ_test.F90 integ.o

have_driver: have_driver.o have_test.o have.o
have_driver.o: have_driver.F90 have_test.o
have_test.o: have_test.F90 have.o

mean_driver: mean_driver.o mean_test.o mean.o
mean_driver.o: mean_driver.F90 mean_test.o
mean_test.o: mean_test.F90 mean.o

runmean_driver: runmean_driver.o runmean_test.o runmean.o
runmean_driver.o: runmean_driver.F90 runmean_test.o
runmean_test.o: runmean_test.F90 runmean.o

diff_mean_driver: diff_mean_driver.o diff_mean_test.o diff_mean.o
diff_mean_driver.o: diff_mean_driver.F90 diff_mean_test.o
diff_mean_test.o: diff_mean_test.F90 diff_mean.o

diff_driver: diff_driver.o diff_test.o diff.o
diff_driver.o: diff_driver.F90 diff_test.o
diff_test.o: diff_test.F90 diff.o

vave_driver: vave_driver.o vave_test.o vave.o
vave_driver.o: vave_driver.F90 vave_test.o
vave_test.o: vave_test.F90 vave.o

barotropic_ke_vol_driver  : barotropic_ke_vol_driver.o barotropic_ke_vol_test.o barotropic_ke_vol.o
barotropic_ke_vol_driver.o: barotropic_ke_vol_driver.F90 barotropic_ke_vol_test.o
barotropic_ke_vol_test.o  : barotropic_ke_vol_test.F90 barotropic_ke_vol.o

integ_vert_driver  : integ_vert_driver.o   integ_vert_test.o integ_vert.o
integ_vert_driver.o: integ_vert_driver.F90 integ_vert_test.o
integ_vert_test.o  : integ_vert_test.F90   integ_vert.o
