function isopycnal(args)

* Ichiro ISHIKAWA  2004-08-06
*
* This script is a modification of zinterp.gs( written by Bob Hart ).

* zgrid: potential density
* zlev: desired density surface
* field: 3-dim field
* r: resulted surface data
zgrid=subwrd(args,1)
zlev=subwrd(args,2)
field=subwrd(args,3)
r=subwrd(args,4)

eps=1.e-5

"q dims"
rec=sublin(result,4)
ztype=subwrd(rec,3)
if (ztype = "fixed") 
   zmin=subwrd(rec,9)
   zmax=zmin
else
   zmin=subwrd(rec,11)
   zmax=subwrd(rec,13)
endif

* Get full z-dimensions of dataset.

*'q file'
*rec=sublin(result,5)
*zsize=subwrd(rec,9)

'set z 'zmin' 'zmax
'zabove=maskout('zgrid','zgrid'-'zlev')'
'zbelow=maskout('zgrid','zlev'-'zgrid')'

'set z 1'

'zabovemin=min(zabove,z='zmin',z='zmax')'
'zbelowmax=max(zbelow,z='zmin',z='zmax')'

'set z 'zmin' 'zmax
'zabove=maskout(zabove,zabovemin-zabove+'eps')'
'zbelow=maskout(zbelow,zbelow-zbelowmax+'eps')'

'set z 'zmin+1' 'zmax
'fabove=maskout('field',zabove)'
'set z 'zmin' 'zmax-1
'fbelow=maskout('field',zbelow)'

* Turn this 3-D grid of values (mostly undefined) into a 2-D height layer
* mean is used here below for simplicity, since mean ignores undefined
* values.

'set z 1'
'zabove=mean(zabove,z='zmin+1',z='zmax')'
'fabove=mean(fabove,z='zmin+1',z='zmax')'
'zbelow=mean(zbelow,z='zmin',z='zmax-1')'
'fbelow=mean(fbelow,z='zmin',z='zmax-1')'

* Finally, interpolate linearly in height and create surface.

'set z 'zmin' 'zmax

'slope=(fabove-fbelow)/(zabove-zbelow)'
'b=fbelow-slope*zbelow'
'define 'r'=slope*'zlev'+b'

'undefine zabove'
'undefine fabove'
'undefine zbelow'
'undefine fbelow'
'undefine slope'
'undefine b'
'undefine zabovemin'
'undefine zbelowmax'
