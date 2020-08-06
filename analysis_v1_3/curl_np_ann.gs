*-------------------------------
'reinit'
*-------------------------------
'open curl_uvsurf_nov1999-oct2009.ctl'
'open curl_uvn_filt_nov1999-oct2009.ctl'
*
'set lon 118 155'
'set lat 20 38'
'set mproj scaled'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'
*-------------------------------
'set vpage 0.0 8.5 0.0 11.0'
'set parea 0.5 4.0 6.0 10.0'
'run colmap.gs rb26'
'set clevs -5.0 -4.0 -3.0 -2.0 -1.5 -1.0 -0.7 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.7 1.0 1.5 2.0 3.0 4.0 5.0'
'set grads off'
'set gxout grfill'
'set dfile 2'
'set xlint 5'
'set ylint 1'
'd curl.1*1e5'
'set gxout contour'
'set clab off'
'set cint 0.2'
'd curl.1*1e5'
splt(22,122,135)
splt(31,122,135)
splt2(122,22,31)
splt2(135,22,31)
label(137.0,39.0,"(a) Curl uo vo [x 1e-5 s-1] (GlobCurrent)",30,0,0.10)
*-------------------------------
'set vpage 0.0 8.5 0.0 11.0'
'set parea 4.5 8.0 6.0 10.0'
'run colmap.gs rb26'
'set clevs -5.0 -4.0 -3.0 -2.0 -1.5 -1.0 -0.7 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.7 1.0 1.5 2.0 3.0 4.0 5.0'
'set grads off'
'set gxout grfill'
'set dfile 1'
'set xlint 5'
'set ylint 1'
'd curl.2*1e5'
'set t 1'
'set gxout contour'
'set clab off'
'set cint 0.2'
'd curl.2*1e5'
splt(22,122,135)
splt(31,122,135)
splt2(122,22,31)
splt2(135,22,31)
label(137.0,39.0,"(b) Curl Ua Va [x 1e-5 s-1] (JRA55-do-v1.3)",30,0,0.10)
'run cbarn2.gs 0.8 0 4.25 5.25 0 2'
*-------------------------------
'set vpage 0.0 8.5 0.0 11.0'
'set parea 1.75 6.75 1.0 4.5'
'set grads off'
'set gxout scatter'
'set lon 122 135'
'set lat 22 31'
'set vrange -3 3'
'set vrange2 -2 2'
*'set dfile 1'
*'set xlint 2'
*'set ylint 1'
*'d curl.2*1e5;curl.1*1e5'
*'d curl.1*1e5;curl.2*1e5'
'd maskout(curl.1*1e5,abs(curl.1)-1e-6);curl.2*1e5'
'query w2xy 2.5 -0.394'
 x1 = subwrd(result,3)
 y1 = subwrd(result,6)
'query w2xy -2.5 0.356'
 x2 = subwrd(result,3)
 y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
'query w2xy -2.0 1.75'
 x1 = subwrd(result,3)
 y1 = subwrd(result,6)
'draw string ' x1 ' ' y1 ' Sw = -0.15'
label(-2.5,2.2,"(c)",5,0,0.10)
label(0.0,-2.5,"Curl uo vo [x 1e-5 s-1] (GlobCurrent)",30,0,0.10)
label(-4.0,0.0,"Curl Ua Va [x 1e-5 s-1] (JRA55-do-v1.3)",30,90,0.10)
*-------------------------------
'printim curl_np_ann.png white'
*-------------------------------
function label(x,y,lab,len,angle,size,justify)
if(size='' | size='size');size=0.10;endif;
if(justify='' | justify='justify');justify='c';endif;
size2=size*1.2
'set strsiz ' size ' ' size2 
'set string 1 ' justify ' 3 ' angle
w = size*len/2
h = (size2*1.4)/2 
'query w2xy ' x ' ' y
 x = subwrd(result,3)
 y = subwrd(result,6)
'set line 0'
if(angle=0)
 'draw recf '%(x-w)%' '%(y-h*1.2)%' '%(x+w)%' '%(y+h)
endif
'draw string ' x ' ' y ' ' lab
'set line 1'
'set string 1 c 3 0' 
*--------------------------------------------
function splt(pos,min,max)
  'set line 3 1 5'
  'query w2xy ' min ' ' pos
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query w2xy ' max ' ' pos
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
*--------------------------------------------
function splt2(pos,min,max)
  'set line 3 1 5'
  'query w2xy ' pos ' ' min
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query w2xy ' pos ' ' max
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
