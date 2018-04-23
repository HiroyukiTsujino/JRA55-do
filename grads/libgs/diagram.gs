'open inv_diag.ctl'

asimef=0.925
t1= 1
t2=73
strsz=0.2

'define apriprod=ave(priprod,t='t1',t='t2')*86400*5'
'define amortp1 =ave(mortp1,t='t1',t='t2' )*86400*5'
'define amortp2 =ave(mortp2,t='t1',t='t2' )*86400*5'
'define agrp2z  =ave(grp2z,t='t1',t='t2'  )*86400*5'
'define amortz  =ave(mortz,t='t1',t='t2'  )*86400*5'
'define aexcrtn =ave(excrtn,t='t1',t='t2'  )*86400*5'
'define aremid  =ave(remid,t='t1',t='t2'  )*86400*5'
'define asink   =ave(sink ,t='t1',t='t2'  )*86400*5'

x1 = 4
x2 = 9
y1 = 2.1
y2 = 7.1
dx = 2.5
dy = 2.5

* Detritus
rc = drawrec ( x1, y1, dx, dy )
'set strsiz 'dx*0.5
'set string 1 c 10 0'
'draw string 'x1' 'y1' D'

* ZooPlankton
rc = drawrec ( x2, y1, dx, dy )
'set strsiz 'dx*0.5
'set string 1 c 10 0'
'draw string 'x2' 'y1' Z'

* Nutrient
rc = drawrec ( x1, y2, dx, dy )
'set strsiz 'dx*0.5
'set string 1 c 10 0'
'draw string 'x1' 'y2' N'

* PhytoPlankton
rc = drawrec ( x2, y2, dx, dy )
'set strsiz 'dx*0.5
'set string 1 c 10 0'
'draw string 'x2' 'y2' P'


* Priprod
xx1=x1+0.5*dx
xx2=x2-0.5*dx
yy1=y2+0.3*dy
yy2=y2+0.3*dy
'set line 1 1 10'
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = 0.5* (xx1+xx2)
ys = yy1 - 0.1
'd apriprod'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* MortP1 (respiration)
xx1=x2-0.5*dx
xx2=x1+0.5*dx
yy1=y2-0.3*dy
yy2=y2-0.3*dy
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = 0.5* (xx1+xx2)
ys = yy1 - 0.1
'd amortp1'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* MortP2 
xx1=x2-0.5*dx
xx2=x1+0.5*dx
yy1=y2-0.5*dy
yy2=y1+0.5*dy
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = 0.5* (xx1+xx2) + 0.5
ys = 0.5* (yy1+yy2) + 0.2
'd amortp2'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* Grp2Z ( P --> Z )
xx1=x2+0.2*dx
xx2=x2+0.2*dx
yy1=y2-0.5*dy
yy2=y1+0.5*dy
'set line 1 1 10'
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = xx2 + 0.3
ys = 0.5* (yy1+yy2)
'd agrp2z'
tmp1=subwrd(result,4) * asimef
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* Grp2Z ( P --> D )
xx1=x2-0.2*dx
xx2=x2-0.7*dx
yy1=y2-0.5*dy
yy2=y1+0.7*dy
'draw line 'xx1' 'yy1' 'xx2' 'yy2
xx1=xx2
yy1=yy2
xx2=x1+0.5*dx
yy2=y1+0.2*dy
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = xx1 + 0.4
ys = yy1 + 0.2
'd agrp2z'
tmp1=subwrd(result,4) * (1-asimef)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* Excrtn
xx1=x2-0.5*dx
yy1=y1+0.2*dy
xx2=x1+0.2*dx
yy2=y2-0.5*dy
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = xx1-0.6
ys = yy1+0.2
'd aexcrtn'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* MortZ
xx1=x2-0.5*dx
yy1=y1-0.2*dy
xx2=x1+0.5*dx
yy2=y1-0.2*dy
'set line 1 1 10'
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = 0.5* (xx1+xx2)
ys = yy1 - 0.1
'd amortz'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* Remid
xx1=x1-0.2*dx
xx2=x1-0.2*dx
yy1=y1+0.5*dy
yy2=y2-0.5*dy
'set line 1 1 10'
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = xx2 + 0.3
ys = 0.5* (yy1+yy2)
'd aremid'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* Sink
xx1=x1
xx2=x1
yy1=y1-0.5*dy
yy2=y1-0.8*dy
'set line 1 1 10'
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )
xs = xx2 + 0.4
ys = 0.7* (yy1+yy2) 
'd asink'
tmp1=-subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2

* Advection ( diagnosed from no3 balance )
xx1 = x1 - 0.75*dx
xx2 = x1 - 0.75*dx
yy1 = y1 - 0.7*dy
yy2 = y2 
'set line 1 1 10'
'draw line 'xx1' 'yy1' 'xx2' 'yy2
xs = xx2 + 0.3
ys = 0.5* (yy1+yy2) 
'd aremid+amortz-apriprod'
tmp1=subwrd(result,4)
tmp2=math_format( "%5.1f", tmp1)
'set strsiz 'strsz
'set string 1 tc 6 0'
'draw string 'xs' 'ys' 'tmp2
xx1 = xx2
yy1 = yy2
xx2 = x1 - 0.5*dx
yy2 = y2 
'draw line 'xx1' 'yy1' 'xx2' 'yy2
rc = arheadrec( xx1, yy1, xx2, yy2, 0.4 )


*==================================================================
* Functions
*==================================================================
*------------------------------------------------------------------
function drawrec ( x0, y0, dx, dy )
*------------------------------------------------------------------
x1 = x0 - 0.5 * dx 
x2 = x0 + 0.5 * dx
y1 = y0 - 0.5 * dx
y2 = y0 + 0.5 * dy
'draw rec 'x1' 'y1' 'x2' 'y2
return

*------------------------------------------------------------------
function arheadrec ( x0, y0, x1, y1, r0 )
*------------------------------------------------------------------
'set line 1 1 100'
r2 = (x1-x0)*(x1-x0) + (y1-y0)*(y1-y0)
*********sqrt *********
* r = sqrt(r2)
*************************
*      'define rr = 'r2
*      'set x  1'
*      'set y  1'
*      'set z  1'
*      'set t  1'
*      'd sqrt(rr)'
*      r = subwrd(result,4)
r = math_sqrt(r2)
************************
 x2 = r0 * (x0-x1) / r + x1
 y2 = r0 * (y0-y1) / r + y1
 sin15 = 0.258819
 cos15 = 0.965926
 x = x2 - x1
 y = y2 - y1
 xt1 =   x * cos15 + y * sin15 + x1
 yt1 = - x * sin15 + y * cos15 + y1
 xt2 =   x * cos15 - y * sin15 + x1
 yt2 =   x * sin15 + y * cos15 + y1
 'set line 1 1 1'
 'draw polyf 'x1' 'y1' 'xt1' 'yt1' 'xt2' 'yt2' 'x1' 'y1
return



 











