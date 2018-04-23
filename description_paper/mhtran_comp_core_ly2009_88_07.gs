*------------------------------------------------
'reinit'
*------------------------------------------------
'open mhtrans-core_aug2017_ly2009.ctl'
'open mhtrans-v0_1noadj.ctl'
'open mhtrans-v1_3noadj.ctl'
*
'open merid_ht_evap.ctl'
'open merid_ht_prec.ctl'
'open merid_ht_river.ctl'
'open merid_ht_sice.ctl'
*-----------------------------------------------
'set vpage 0.0 11.0 0.0 8.5'
'set parea 0.6 3.6 1.0 8.0'
'set gxout line'
'set grads off'
'set xlopts 1 3 0.08'
'set ylopts 1 3 0.12'
'set xlab on'
'set ylab on'
'set vrange -2 2.5'
'set lat -90 90'
'set xlint 15'
'set ylint 0.5'
*
'define glbadj=ave(-glb.4+glb.5+glb.6+glb.7,time=jan1948,time=jan2007)*1e-15'
'set ccolor 3'
'set cthick 10'
'set cmark 0'
*'d ave(glb.1,time=jan1988,time=jan2007)*1e-15 + glbadj'
'd ave(glb.1,time=jan1988,time=jan2007)*1e-15'
'set ccolor 4'
'set cthick 10'
'set cmark 0'
'd ave(glb.2,time=jan1988,time=jan2007)*1e-15 + glbadj'
'set ccolor 2'
'set cthick 10'
'set cmark 0'
'd ave(glb.3,time=jan1988,time=jan2007)*1e-15 + glbadj'
label(0.0,2.65,"GLOBAL MHT (1988-2007)",30,0,0.12)
label(-110,0.0,"[PW]",4,90,0.10)
label(0.0,-2.2,"latitude",30,0,0.12)
*
obswitherr( 47.5, 0.68, 0.15, 4, 0.08)
obswitherr( 36.0, 1.11, 0.37, 4, 0.08)
obswitherr( 24.0, 1.62, 0.40, 4, 0.08)
obswitherr(  9.5, 1.50, 1.54, 4, 0.08)
obswitherr(-10.5, 0.55, 1.45, 4, 0.08)
obswitherr(-19.5,-0.43, 0.61, 4, 0.08)
obswitherr(-31.0,-0.51, 0.39, 4, 0.08)
*
splt(0.0,0,180)
*---------------------------------------------------------------
*
'set line 3 1 5'
'run drawline.gs -80.0 2.35 -55.0 2.35'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.35 CORE'
'set string 1 c 3'
*
'set line 4 1 5'
'run drawline.gs -80.0 2.2 -55.0 2.2'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.2 JRA55-raw'
'set string 1 c 3'
*
'set line 2 1 5'
'run drawline.gs -80.0 2.05 -55.0 2.05'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.05 JRA55-do'
'set string 1 c 3'
*
*---------------------------------------------------------------
'set vpage 0.0 11.0 0.0 8.5'
'set parea 4.2 7.2 1.0 8.0'
'set gxout line'
'set grads off'
'set xlopts 1 3 0.08'
'set ylopts 1 3 0.12'
'set xlab on'
'set ylab on'
'set vrange -2 2.5'
'set lat -90 90'
'set xlint 15'
'set ylint 0.5'
*
'define inpadj=ave(-inp.4+inp.5+inp.6+inp.7,time=jan1948,time=jan2007)*1e-15'
'set ccolor 3'
'set cthick 10'
'set cmark 0'
*'d ave(inp.1,time=jan1988,time=jan2007)*1e-15 + inpadj'
'd ave(inp.1,time=jan1988,time=jan2007)*1e-15'
'set ccolor 4'
'set cthick 10'
'set cmark 0'
'd ave(inp.2,time=jan1988,time=jan2007)*1e-15 + inpadj'
'set ccolor 2'
'set cthick 10'
'set cmark 0'
'd ave(inp.3,time=jan1988,time=jan2007)*1e-15 + inpadj'
label(0.0,2.65,"Indo-Pacific MHT (1988-2007)",30,0,0.12)
label(-110,0.0,"[PW]",4,90,0.10)
label(0.0,-2.2,"latitude",30,0,0.12)
*
splt(0.0,0,180)
*
obswitherr( 47.5, 0.04, 0.16, 4, 0.08)
obswitherr( 23.0, 0.64, 0.29, 4, 0.08)
obswitherr( 10.5, 0.51, 1.22, 4, 0.08)
obswitherr(-18.5,-1.15, 0.61, 4, 0.08)
obswitherr(-29.5,-0.91, 0.36, 4, 0.08)
*---------------------------------------------------------------
*
'set line 3 1 5'
'run drawline.gs -80.0 2.35 -55.0 2.35'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.35 CORE'
'set string 1 c 3'
*
'set line 4 1 5'
'run drawline.gs -80.0 2.2 -55.0 2.2'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.2 JRA55-raw'
'set string 1 c 3'
*
'set line 2 1 5'
'run drawline.gs -80.0 2.05 -55.0 2.05'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.05 JRA55-do'
'set string 1 c 3'
*
*---------------------------------------------------------------
'set vpage 0.0 11.0 0.0 8.5'
'set parea 7.8 10.8 1.0 8.0'
'set gxout line'
'set grads off'
'set xlopts 1 3 0.08'
'set ylopts 1 3 0.12'
'set xlab on'
'set ylab on'
'set vrange -2 2.5'
'set lat -90 90'
'set xlint 15'
'set ylint 0.5'
*
'define atladj=ave(-atl.4+atl.5+atl.6+atl.7,time=jan1948,time=jan2007)*1e-15'
'set ccolor 3'
'set cthick 10'
'set cmark 0'
*'d ave(atl.1,time=jan1988,time=jan2007)*1e-15 + atladj'
'd ave(atl.1,time=jan1988,time=jan2007)*1e-15'
'set ccolor 4'
'set cthick 10'
'set cmark 0'
'd ave(atl.2,time=jan1988,time=jan2007)*1e-15 + atladj'
'set ccolor 2'
'set cthick 10'
'set cmark 0'
'd ave(atl.3,time=jan1988,time=jan2007)*1e-15 + atladj'
label(0.0,2.65,"Arctic-Atlantic MHT (1988-2007)",30,0,0.12)
label(-110,0.0,"[PW]",4,90,0.10)
label(0.0,-2.2,"latitude",30,0,0.12)
*
splt(0.0,0,180)
*
obswitherr( 46.0, 0.58, 0.24, 4, 0.08)
obswitherr( 37.0, 0.88, 0.22, 4, 0.08)
obswitherr( 25.0, 1.20, 0.27, 4, 0.08)
obswitherr( 10.5, 1.07, 0.33, 4, 0.08)
obswitherr(-11.5, 0.56, 0.26, 4, 0.08)
obswitherr(-32.0, 0.34, 0.18, 4, 0.08)
obswitherr( 26.5, 1.24, 0.33, 2, 0.10)
*---------------------------------------------------------------
*
'set line 3 1 5'
'run drawline.gs -80.0 2.35 -55.0 2.35'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.35 CORE'
'set string 1 c 3'
*
'set line 4 1 5'
'run drawline.gs -80.0 2.2 -55.0 2.2'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.2 JRA55-raw'
'set string 1 c 3'
*
'set line 2 1 5'
'run drawline.gs -80.0 2.05 -55.0 2.05'
'set line 1 1 1'
'set string 1 l 3'
'set strsiz 0.10'
'run drawstring.gs -50.0 2.05 JRA55-do'
'set string 1 c 3'
*
*---------------------------------------------------------------
'close 7'
'close 6'
'close 5'
'close 4'
'close 3'
'close 2'
'close 1'
*--------------------------------------------------------------
'enable print dswrf.gm'
'print'
'disable print'
'!gxeps -c -i dswrf.gm -o mht_comp_core_ly2009_88_07.eps'
'!rm -f dswrf.gm'
*==============================================================
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
*--------------------------------------------
function obswitherr(posx,posy,erry,mrk,msiz)
  'set line 1 1 4'
  'query w2xy ' posx ' ' posy+erry
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
  'query w2xy ' posx ' ' posy-erry
  x2 = subwrd(result,3)
  y2 = subwrd(result,6)
'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
  'query w2xy ' posx ' ' posy
  x1 = subwrd(result,3)
  y1 = subwrd(result,6)
'draw mark ' mrk ' ' x1 ' ' y1 ' ' msiz
