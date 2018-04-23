function ogpdens(args)

* Ichiro ISHIKAWA  2004-08-06

* t: potential temperature (degC)
* s: salinity (PSU)
* sigma: resulted potential density
t=subwrd(args,1)
s=subwrd(args,2)
sigma=subwrd(args,3)

dca1=-0.157406e0
dca2= 6.793952e-2
dca3=-9.095290e-3
dca4= 1.001685e-4
dca5=-1.120083e-6
dca6= 6.536332e-9

dcb1= 0.824493e0
dcb2=-4.0899e-3
dcb3= 7.6438e-5
dcb4=-8.2467e-7
dcb5= 5.3875e-9

dcc1=-5.72466e-3
dcc2= 1.0227e-4
dcc3=-1.6546e-6

dcd= 4.8314e-4

dce1= 19659.35e0
dce2=   144.5863e0
dce3=    -1.722523e0
dce4=     1.019238e-2
dce5=    -4.768276e-5

dcf1= 52.85624e0
dcf2= -3.128126e-1
dcf3=  6.456036e-3
dcf4= -5.370396e-5
dcf5=  3.884013e-1
dcf6=  9.116446e-3
dcf7= -4.628163e-4

dcg1=  3.185918e0
dcg2=  2.189412e-2
dcg3= -2.823685e-4
dcg4=  1.715739e-6
dcg5=  6.703377e-3
dcg6= -1.839953e-4
dcg7=  1.912264e-7
dcg8=  1.477291e-4

dch1=  2.111102e-4
dch2= -1.196438e-5
dch3=  1.364330e-7
dch4= -2.048755e-6
dch5=  6.375979e-8
dch6=  5.240967e-10

sigma'='dca1'+('dca2'+('dca3'+('dca4'+('dca5'+'dca6'*'t')*'t')*'t')*'t')*'t'+'s'*('dcb1'+('dcb2'+('dcb3'+('dcb4'+'dcb5'*'t')*'t')*'t')*'t'+sqrt('s')*('dcc1'+('dcc2'+'dcc3'*'t')*'t')+'s'*'dcd')'
