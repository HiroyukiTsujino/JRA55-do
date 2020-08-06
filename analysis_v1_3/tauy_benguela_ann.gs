*-------------------------------
function main(args)
*-------------------------------
version=subwrd(args,1)
if(version="")
 say ' Usage: tauy_atl_ann #version'
 exit
endif
say ' Forcing: 'type
*-------------------------------
varscow.1=January
varscow.2=February
varscow.3=March
varscow.4=April
varscow.5=May
varscow.6=June
varscow.7=July
varscow.8=August
varscow.9=September
varscow.10=October
varscow.11=November
varscow.12=December
*-------------------------------
'reinit'
*-------------------------------
'sdfopen /work116/htsujino/SCOW/wind_stress_meridional_monthly_maps.nc'
'open tauy_'version'.ctl'
'set lon 0 30'
'set lat -40 -10'
'set xlint 20'
'set ylint 10'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'
*-------------------------------
'set vpage 0.0 11.0 0.0 8.5'
'set parea 0.5 5.0 1.0 8.0'
'run clevsreg.gs -0.02 0.125 0.005'
'run colmap.gs odcm3'
'set grads off'
'set gxout grfill'
'set poli off'
'set mpdset hires'
'set dfile 2'
'd ave(tauy,time=nov1999,time=oct2009)'
'set gxout contour'
'set cint 0.01'
'd ave(tauy,time=nov1999,time=oct2009)'
label(15.0,-7.5,"TAUY annual mean [N/m] ("version") [Nov.1999-Oct.2009]",60,0,0.10)
*-------------------------------
'set vpage 0.0 11.0 0.0 8.5'
'set parea 6.0 10.5 1.0 8.0'
'run clevsreg.gs -0.02 0.125 0.005'
'run colmap.gs odcm3'
'set grads off'
'set gxout grfill'
'set dfile 1'
'set t 1'
'd maskout((january+february+march+april+may+june+july+august+september+october+november+december)/12,january+9000)'
'set gxout contour'
'set cint 0.01'
'd maskout((january+february+march+april+may+june+july+august+september+october+november+december)/12,january+9000)'
label(15.0,-7.5,"TAUY annual mean [N/m] (SCOW) [Sep.1999-Oct.2009]",60,0,0.10)
'run cbarn2.gs 0.8 0 5.25 0.5 0 2'
*-------------------------------
filem="curl.gm"
fileo="tauy_benguela_ann_"version".eps"
filep="tauy_benguela_ann_"version".png"
'printim 'filep' white'
'enable print 'filem
'print'
'disable print'
'!gxeps -c -i 'filem' -o 'fileo
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
