
targets_test := fft_pgi_driver power_driver lowpass_driver

test: $(targets_test)

vpath %_test.F90 test
vpath %_driver.F90 test

fft_pgi_driver:   fft_pgi_driver.o   fft_pgi_test.o fft_pgi.o
fft_pgi_driver.o: fft_pgi_driver.F90 fft_pgi_test.o
fft_pgi_test.o:   fft_pgi_test.F90   fft_pgi.o

power_driver:   power_driver.o   power_test.o power.o
power_driver.o: power_driver.F90 power_test.o
power_test.o:   power_test.F90   power.o

lowpass_driver:   lowpass_driver.o   lowpass_test.o lowpass.o
lowpass_driver.o: lowpass_driver.F90 lowpass_test.o
lowpass_test.o:   lowpass_test.F90   lowpass.o
