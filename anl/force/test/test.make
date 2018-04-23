
targets_test := force_data_driver remap_driver interpolate_driver coastline_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

force_data_driver:   force_data_driver.o   force_data_test.o force_data.o
force_data_driver.o: force_data_driver.F90 force_data_test.o
force_data_test.o:   force_data_test.F90   force_data.o

remap_driver:   remap_driver.o   remap_test.o remap.o
remap_driver.o: remap_driver.F90 remap_test.o
remap_test.o:   remap_test.F90   remap.o

interpolate_driver:   interpolate_driver.o   interpolate_test.o interpolate.o
interpolate_driver.o: interpolate_driver.F90 interpolate_test.o
interpolate_test.o:   interpolate_test.F90   interpolate.o

coastline_driver:   coastline_driver.o   coastline_test.o coastline.o
coastline_driver.o: coastline_driver.F90 coastline_test.o
coastline_test.o:   coastline_test.F90   coastline.o
