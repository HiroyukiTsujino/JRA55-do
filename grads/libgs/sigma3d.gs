function sigma3d(args)

* Ichiro ISHIKAWA  2004-08-06

* t: potential temperature (degC)
* s: salinity (PSU)
* sigma: resulted potential density
t=subwrd(args,1)
s=subwrd(args,2)
sigma=subwrd(args,3)

"q dims"
line1=sublin(result,4)
ztype=subwrd(line1,3)
if (ztype = "fixed") 
   zmin=subwrd(line1,9)
   zmax=zmin
else
   zmin=subwrd(line1,11)
   zmax=subwrd(line1,13)
endif
*'q file'
*rec=sublin(result,5)
*zsize=subwrd(rec,9)

'set z 'zmin' 'zmax
'run ogpdens 't' 's' sigma'
