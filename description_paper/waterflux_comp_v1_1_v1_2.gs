*------------------------------------------------
'reinit'
*------------------------------------------------
*
'open evapor_glb_v1_1.ctl'
'open evapor_glb_v1_2.ctl'
'open evapor_glb_core_aug2017.ctl'
*
'open precip_glb_v1_1.ctl'
'open precip_glb_v1_2.ctl'
'open precip_glb_core_aug2017.ctl'
'open gpcp_precip_glb.ctl'
*
'open evapor_glb_v1_1.old.ctl'
*
*'open runoff_all_annual.ctl'
*'open core_greenland.ctl'
*
'set vpage 0.0 8.5 0.0 11.0'
'set parea 1.0 8.0 7.7 10.7'
'set gxout line'
'set grads off'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
'set xlab on'
'set ylab on'
'set ylab %.1f'
'set vrange 13.1 15.1'
'set ylint 0.4'
'set time jan1958 jan2016'
*
'set ccolor 2'
'set cthick 5'
'set cmark 0'
'd f.2*3.654e5'
*
'set ccolor 4'
'set cthick 5'
'set cmark 0'
'd f.1*3.654e5'
*
'set ccolor 5'
'set cthick 5'
'set cmark 0'
'd f.8*3.654e5'
*
*'set ccolor 3'
*'set cthick 5'
*'set cmark 0'
*'d f.3*3.654e5'
*
label(29,15.2,"(a) Evaporation",30,0,0.12)
*label(29,3.95,"Precip (blue)  Evap (red)",30,0,0.12)
*label(-5,14,"[Sv]",4,90,0.12)
label(-4,14.0,"10^9 [kg/sec]",4,90,0.12)
*label(29,-25,"[Year]",30,0,0.12)
*--------------
* line legend
'set line 4 1 10'
'run drawline_gr2xy.gs 3 13.7 8 13.7'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 9 13.7 JRA55-do-v1.1'
'set string 1 c 3'
*
'set line 5 1 10'
'run drawline_gr2xy.gs 3 13.5 8 13.5'
'set line 1 1 1'
'set string 1 l 3'
'query gr2xy 9 13.5'
xp = subwrd(result,3)
yp = subwrd(result,6)
label_tmp="JRA55-do-v1.1 with the older treatment on scalar wind"
'draw string 'xp ' 'yp' 'label_tmp
'set string 1 c 3'
*
'set line 2 1 10'
'run drawline_gr2xy.gs 3 13.30 8 13.30'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 9 13.3 JRA55-do-v1.2'
'set string 1 c 3'
*
*----------------------------------------------------------
*
'set vpage 0.0 8.5 0.0 11.0'
'set parea 1.0 8.0 4.1 7.1'
'set gxout line'
'set grads off'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
'set ylab %.1f'
'set xlab on'
'set ylab on'
'set vrange 11.9 14.1'
'set ylint 0.4'
'set time jan1958 jan2016'
*
'set ccolor 4'
'set cthick 5'
'set cmark 0'
'set cstyle 1'
'd f.4*3.654e5'
*
'set ccolor 1'
'set cthick 5'
'set cstyle 1'
'set cmark 0'
'd f.7*3.654e5'
*
'set ccolor 2'
'set cthick 5'
'set cstyle 1'
'set cmark 0'
'd f.5*3.654e5'
*
*'set ccolor 3'
*'set cthick 5'
*'set cstyle 1'
*'set cmark 0'
*'d f.6*3.654e5'
*
label(29,14.2,"(b) Precipitation",30,0,0.12)
*label(-5,0,"[Sv]",4,90,0.12)
label(-4,13.2,"10^9 [kg/sec]",4,90,0.12)
*label(34,0.0,"[Year]",30,0,0.12)
*
*--------------
* line legend
'set line 1 1 10'
'run drawline_gr2xy.gs 3 12.7 8 12.7'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 9 12.7 GPCP-v2.3'
'set string 1 c 3'
*
'set line 4 1 10'
'run drawline_gr2xy.gs 3 12.5 8 12.5'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 9 12.5 JRA55-do-v1.1'
'set string 1 c 3'
*
'set line 2 1 10'
'run drawline_gr2xy.gs 3 12.3 8 12.3'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 9 12.3 JRA55-do-v1.2'
'set string 1 c 3'
*
*'set line 3 1 10'
*'run drawline_gr2xy.gs 3 12.1 8 12.1'
*'set line 1 1 1'
*'set string 1 l 3'
*'run drawstring_gr2xy.gs 9 12.1 CORE'
*'set string 1 c 3'
*----------------------------------------------------------
*
'set vpage 0.0 8.5 0.0 11.0'
'set parea 1.0 8.0 0.5 3.5'
'set gxout line'
'set grads off'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
'set ylab %.1f'
'set xlab on'
'set ylab on'
'set vrange 0.3 2.1'
'set ylint 0.2'
'set time jan1958 jan2016'
*
'set ccolor 2'
'set cthick 5'
'set cstyle 1'
'set cmark 0'
'd (f.2-f.5)*3.654e5'
*
'set ccolor 4'
'set cthick 5'
'set cstyle 1'
'set cmark 0'
'd (f.1-f.4)*3.654e5'
*
*'set ccolor 3'
*'set cthick 5'
*'set cstyle 1'
*'set cmark 0'
*'d (f.3-f.6)*3.654e5'
*
*label(29,3.5,"E-P",30,0,0.12)
label(29,2.2,"(c) E - P ",30,0,0.12)
*label(-5,0,"[Sv]",4,90,0.12)
label(-4,1.2,"10^9 [kg/sec]",4,90,0.12)
label(29,0.1,"[Year]",30,0,0.12)
*--------------
* line legend
'set line 4 1 10'
'run drawline_gr2xy.gs 3 0.45 8 0.45'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 9 0.45 JRA55-do-v1.1'
'set string 1 c 3'
*
'set line 2 1 10'
'run drawline_gr2xy.gs 25 0.45 30 0.45'
'set line 1 1 1'
'set string 1 l 3'
'run drawstring_gr2xy.gs 31 0.45 JRA55-do-v1.2'
'set string 1 c 3'
*
*'set line 3 1 10'
*'run drawline_gr2xy.gs 47 0.45 52 0.45'
*'set line 1 1 1'
*'set string 1 l 3'
*'run drawstring_gr2xy.gs 53 0.45 CORE'
*'set string 1 c 3'
*
*'close 9'
'close 8'
'close 7'
'close 6'
'close 5'
'close 4'
'close 3'
'close 2'
'close 1'
*----------------------------------------------------
'enable print waterflux.gm'
'print'
'disable print'
'!gxeps -c -i waterflux.gm -o waterflux_jra55_v1_1_v1_2.eps'
'!rm -f waterflux.gm'
*====================================================
function bottom(fname,i,max)
 result=read(fname)
  pos=sublin(result,2)
 'query w2xy ' subwrd(pos,i) ' ' subwrd(pos,5)
 x = subwrd(result,3)
 y = subwrd(result,6)
 'query w2xy ' subwrd(pos,2) ' ' max
 clip= subwrd(result,6)
while(1)
  result=read(fname)
  ret=sublin(result,1)
  if(ret=2);break;endif;
  pos=sublin(result,2)
 'query w2xy ' subwrd(pos,i) ' ' subwrd(pos,5)
 x1 = subwrd(result,3)
 y1 = subwrd(result,6)
 if(y<clip | y1<clip)
   if(y>clip)
     x2=(x1-x)*(clip-y)/(y1-y)+x
     'draw line ' x ' ' y ' ' x2 ' ' clip
   endif
   if(y1>clip)
     x2=(x1-x)*(clip-y)/(y1-y)+x
     'draw line ' x1 ' ' y1 ' ' x2 ' ' clip
   endif
 else
   'draw line ' x ' ' y ' ' x1 ' ' y1
 endif
 x = x1
 y = y1
endwhile
ret=close(fname)
*------------------------------------------
function shadeset(type)
*  Temperature
 'set rgb 40   0   0   0'
 'set rgb 41  40  40  40'
*      -  1.2
 'set rgb 42  60  60  60'
*  1.2 -  2.0
 'set rgb 43  80  80  80'
*  2.0 -  5.0
 'set rgb 44 100 100 100'
*  5.0 - 15.0
 'set rgb 45 120 120 120'
* 15.0 - 20.0
 'set rgb 46 140 140 140'
* 20.0 - 25.0
 'set rgb 47 160 160 160'
* 25.0 - 
 'set rbcols 41 42 43 44 45 46 47'
 if(type='anom')
 'set rbcols 0 41'
 'set clevs 0.0 '
else
 if(type='delta')
 'set rbcols 41 42 44  46 47 '
 'set clevs 1.6 1.8 2.2 2.4 '
else
 if(type='none')
 'set rbcols 0 41 '
 'set clevs 999.0 '
 else
* 'set clevs 1.2 2.0 5.0 15.0 20.0 25.0'
 'set rbcols 40 42'
 'set clevs 28'
  endif
 endif
endif
*------------------------------------------
function shade2(type)
*  Temperature
 'set rgb 40   0   0   0'
 'set rgb 41  40  40  40'
*      -  1.2
 'set rgb 42  60  60  60'
*  1.2 -  2.0
 'set rgb 43  80  80  80'
*  2.0 -  5.0
 'set rgb 44 100 100 100'
*  5.0 - 15.0
 'set rgb 45 120 120 120'
* 15.0 - 20.0
 'set rgb 46 140 140 140'
* 20.0 - 25.0
 'set rgb 47 160 160 160'
* 25.0 - 
 'set rbcols 41 42 43 44 45 46 47'
 if(type='anom')
 'set rbcols 41 0'
 'set clevs 0.0 '
else
 if(type='delta')
 'set rbcols 41 42 44  46 47 '
 'set clevs 1.6 1.8 2.2 2.4 '
 else
* 'set clevs 1.2 2.0 5.0 15.0 20.0 25.0'
 'set rbcols 40 42'
 'set clevs 28'
 endif
endif
*-----------------------------------------
function label2(x,y,lab,len,angle)
'set strsiz 0.12 0.15' 
'set string 1 c 3 ' angle
w = 0.10*len/2 + 0.01
h = 0.11/2 + 0.02
'query w2xy ' x ' ' y
 x = subwrd(result,3)
 y = subwrd(result,6)
'set line 0'
'draw recf '%(x-w)%' '%(y-h)%' '%(x+w)%' '%(y+h)
'draw string ' x ' ' y ' ' lab
'set line 1'
'set string 1 c 3 0' 
*------------------------------------------
function tick(fname,i)
while(1)
  result=read(fname)
  ret=sublin(result,1)
  if(ret=2);break;endif;
  pos=sublin(result,2)
 'query w2xy ' subwrd(pos,i) ' 0 '
  x = subwrd(result,3)
  y = subwrd(result,6)
 'draw line ' x ' ' y+0.02 ' ' x ' ' y+0.12
endwhile
ret=close(fname)
*--------------------------------------------
function scale2(min,max)
  'set strsiz 0.12'
  'set string 1 c'
  'query w2xy ' min ' -60 '
  x = subwrd(result,3)
  y = subwrd(result,6)
'draw string ' x ' ' y+0.1 ' ' min 'N'
  'query w2xy ' max ' -60 '
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
'draw string ' x1 ' ' y1+0.1 ' ' max 'N'
 'draw line ' x ' ' y ' ' x1 ' ' y1
  i=min
  while(i<=max)
    'query w2xy ' i ' -60 '
    x = subwrd(result,3)
    y = subwrd(result,6)
   'draw line ' x ' ' y-0.08 ' ' x ' ' y
   if (i=5 | i=10 | i=15 | i=20 | i=25 | i=30 )
     'draw string ' x ' ' y+0.1 ' ' i
   endif
  i = i + 1
  endwhile
*-----------------------------------------
function label3(x,y,lab,len,angle)
'set strsiz 0.07 0.10' 
'set string 1 c 3 ' angle
w = 0.10*len/2 + 0.01
h = 0.11/2 + 0.02
'query w2xy ' x ' ' y
 x = subwrd(result,3)
 y = subwrd(result,6)
'set line 0'
'draw recf '%(x-w)%' '%(y-h)%' '%(x+w)%' '%(y+h)
'draw string ' x ' ' y ' ' lab
'set line 1'
'set string 1 c 3 0' 
*------------------------------------------
function tick2(fname,i,tval)
while(1)
  result=read(fname)
  ret=sublin(result,1)
  if(ret=2);break;endif;
  pos=sublin(result,2)
 'query w2xy ' subwrd(pos,i) ' ' tval
  x = subwrd(result,3)
  y = subwrd(result,6)
 'draw line ' x ' ' y+0.02 ' ' x ' ' y+0.12
endwhile
ret=close(fname)
*--------------------------------------------------------
function ytick(min,max,int,pos)
i=min
while(i<=max)
 'query w2xy ' pos ' ' i
    x = subwrd(result,3)
    y = subwrd(result,6)
 'draw line ' x-0.08 ' ' y ' ' x ' ' y
  i=i+int
endwhile
*-----------------------------------------
function axislab(x,y,lab,len,angle)
'set strsiz 0.09 0.12' 
'set string 1 c 3 ' angle
w = 0.10*len/2 + 0.01
h = 0.11/2 + 0.02
'query w2xy ' x ' ' y
 x = subwrd(result,3)
 y = subwrd(result,6)
'set line 0'
'draw recf '%(x-w)%' '%(y-h)%' '%(x+w)%' '%(y+h)
'draw string ' x ' ' y ' ' lab
'set line 1'
'set string 1 c 3 0'
*--------------------------------------------------
function dlabel(fname)
while(1)
 result=read(fname)
 ret=sublin(result,1)
 if(ret!=0);break;endif;
 pos=sublin(result,2)
 drwlbl(pos)
endwhile
if(ret!=1)
 ret=close(fname)
endif
*--------------------------------------------------
function drwlbl(line)
 lat=subwrd(line,1)
 comment=substr(lat,1,1)
 if(comment='#');return;endif
* say line
 dep=subwrd(line,2)
 lab=subwrd(line,3)
 len=subwrd(line,4)
 angle=subwrd(line,5)
 size=subwrd(line,6)
 label(lat,dep,lab,len,angle,size)
*-----------------------------------------
function label(x,y,lab,len,angle,size,justify)
if(size='' | size='size');size=0.10;endif;
if(justify='' | justify='justify');justify='c';endif;
size2=size*1.2
'set strsiz ' size ' ' size2 
'set string 1 ' justify ' 3 ' angle
w = size*len/2
h = (size2*1.4)/2 
'query gr2xy ' x ' ' y
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
  'set line 1 1 4'
  'query gr2xy ' min ' ' pos
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query gr2xy ' max ' ' pos
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
*--------------------------------------------
function splt2(pos,min,max)
  'set line 1 1 4'
  'query gr2xy ' pos ' ' min
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query gr2xy ' pos ' ' max
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
