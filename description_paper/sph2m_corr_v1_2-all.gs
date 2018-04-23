*------------------------------------------------
'reinit'
*------------------------------------------------
'open sph2m_corr_v1_2.1958_1972.ctl'
'open jra55_ocean_mask_undef0.ctl'
'open jra55_ocean_mask.ctl'
*
'set vpage 0.00 8.50 0.00 11.00'
'set parea 1.50 7.00 7.70 10.70'
*
'set lon 25 385'
'set xlint 60'
'set ylint 20'
'set grid on'
'set mpdraw off'
'set mproj scaled'
*'set mpdset mres'
'set poli off'
'set map 1 1 1'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
*
'set undef file 2'
'set gxout shaded'
*'run colmap.gs odcm3'
*'run clevsreg.gs 0.86 1.15 0.01'
'run colmap.gs factor20'
'set clevs 0.82 0.84 0.86 0.88 0.90 0.92 0.94 0.96 0.98 1.0 1.02 1.04 1.06 1.08 1.10 1.12 1.14 1.16 1.18'
'set grads off'
'd ave(corr_sph,t=1,t=12)*index.2'
*
*'set gxout contour'
*'set ccolor 1'
*'set clab off'
*'set cint 0.02'
*'set cthick 1'
*'set grads off'
*'d sph2m_corr'
*
'set gxout contour'
'set ccolor 1'
'set clab off'
'set cint 0.1'
'set cthick 3'
'set grads off'
'd ave(corr_sph,t=1,t=12)*index.2'
*
'set gxout fgrid'
'set fgvals 0 15'
'd index.3'
*
label(205.0,98.0,"(a) Q2m multiplicative factor (1958-1972)",30,0,0.135)
*'run cbarn2.gs 0.8 0 4.25 6.0 4 5'
'set string 1 c 3'
'run drawstring.gs 70.0 50.0 CI=0.1'
*
'close 3'
'close 2'
'close 1'
*----------------------------------------------------
'open sph2m_corr_v1_2.1973_1996.ctl'
'open jra55_ocean_mask_undef0.ctl'
'open jra55_ocean_mask.ctl'
*
'set vpage 0.00 8.50 0.00 11.00'
'set parea 1.50 7.00 4.10 7.10'
*
'set lon 25 385'
'set xlint 60'
'set ylint 20'
'set grid on'
'set mproj scaled'
'set mpdraw off'
*'set mpdset mres'
'set poli off'
'set map 1 1 1'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
*
'set undef file 2'
'set gxout shaded'
*'run colmap.gs odcm3'
*'run clevsreg.gs 0.86 1.15 0.01'
'run colmap.gs factor20'
'set clevs 0.82 0.84 0.86 0.88 0.90 0.92 0.94 0.96 0.98 1.0 1.02 1.04 1.06 1.08 1.10 1.12 1.14 1.16 1.18'
'set grads off'
'd ave(corr_sph,t=1,t=12)*index.2'
*
*'set gxout contour'
*'set ccolor 1'
*'set clab off'
*'set cint 0.02'
*'set cthick 1'
*'set grads off'
*'d sph2m_corr'
*
'set gxout contour'
'set ccolor 1'
'set clab off'
'set cint 0.1'
'set cthick 3'
'set grads off'
'd ave(corr_sph,t=1,t=12)*index.2'
*
'set gxout fgrid'
'set fgvals 0 15'
'd index.3'
*
label(205.0,98.0,"(b) Q2m multiplicative factor (1973-1997)",30,0,0.135)
*'run cbarn2.gs 0.8 0 4.25 6.0 4 5'
'set string 1 c 3'
'run drawstring.gs 70.0 50.0 CI=0.1'
*
'close 3'
'close 2'
'close 1'
*----------------------------------------------------
'open sph2m_corr_v1_2.1999_2015.ctl'
'open jra55_ocean_mask_undef0.ctl'
'open jra55_ocean_mask.ctl'
*
'set vpage 0.00 8.50 0.00 11.00'
'set parea 1.50 7.00 0.40 3.40'
*
'set lon 25 385'
'set xlint 60'
'set ylint 20'
'set grid on'
'set mproj scaled'
'set mpdraw off'
*'set mpdset mres'
*'set poli off'
'set map 1 1 1'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
*
'set undef file 2'
'set gxout shaded'
*'run colmap.gs odcm3'
*'run clevsreg.gs 0.86 1.15 0.01'
'run colmap.gs factor20'
'set clevs 0.82 0.84 0.86 0.88 0.90 0.92 0.94 0.96 0.98 1.0 1.02 1.04 1.06 1.08 1.10 1.12 1.14 1.16 1.18'
'set grads off'
'd ave(corr_sph,t=1,t=12)*index.2'
*
*'set gxout contour'
*'set ccolor 1'
*'set clab off'
*'set cint 0.02'
*'set cthick 1'
*'set grads off'
*'d sph2m_corr'
*
'set gxout contour'
'set ccolor 1'
'set clab off'
'set cint 0.1'
'set cthick 3'
'set grads off'
'd ave(corr_sph,t=1,t=12)*index.2'
*
'set gxout fgrid'
'set fgvals 0 15'
'd index.3'
*
label(205.0,98.0,"(c) Q2m multiplicative factor (1998-present)",30,0,0.135)
'set string 1 c 3'
'run drawstring.gs 70.0 50.0 CI=0.1'
*
'run cbarn2.gs 0.7 -1 7.5 5.25 4 5'
*
'close 3'
'close 2'
'close 1'
*----------------------------------------------------
'enable print sph2m_corr.gm'
'print'
'disable print'
'!gxeps -c -i sph2m_corr.gm -o sph2m_corr-all.eps'
'!rm sph2m_corr.gm'
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
function spltx(pos,min,max)
  'set line 1 1 4'
  'query gr2xy ' min ' ' pos
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query gr2xy ' max ' ' pos
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
*--------------------------------------------
function splty(pos,min,max)
  'set line 1 1 4'
  'query gr2xy ' pos ' ' min
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query gr2xy ' pos ' ' max
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
