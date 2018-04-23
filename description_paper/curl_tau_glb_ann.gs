*-------------------------------
function main(args)
*-------------------------------
version=subwrd(args,1)
if(version="")
 say ' Usage: curl_tau_nh_ann #version'
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
filem="curl.gm"
fileo="curl_glb_ann_"version".eps"
*-------------------------------
'reinit'
'enable print 'filem
*-------------------------------
'sdfopen /work116/htsujino/SCOW/wind_stress_curl_monthly_maps.nc'
'open hcurl_'version'.ctl'
'set lon 25 385'
'set lat -75 75'
'set xlint 20'
'set ylint 10'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'
*-------------------------------
'set vpage 0.0 8.5 0.0 11.0'
'set parea 0.5 8.0 6.0 10.5'
'run colmap.gs rb26'
'set clevs -5.0 -4.0 -3.0 -2.0 -1.5 -1.0 -0.7 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.7 1.0 1.5 2.0 3.0 4.0 5.0'
'set grads off'
'set gxout grfill'
'set dfile 2'
'd ave(curl*1e7,time=nov1999,time=oct2009)'
label(190.0,80.0,"Curl tau annual mean [x 1e-7 N/m3] ("version") [Nov.1999-Oct.2009]",60,0,0.10)
*-------------------------------
'set vpage 0.0 8.5 0.0 11.0'
'set parea 0.5 8.0 1.0 5.5'
'run colmap.gs rb26'
'set clevs -5.0 -4.0 -3.0 -2.0 -1.5 -1.0 -0.7 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.7 1.0 1.5 2.0 3.0 4.0 5.0'
'set grads off'
'set gxout grfill'
'set dfile 1'
'set t 1'
'd maskout((january+february+march+april+may+june+july+august+september+october+november+december)/12,january+9000)'
label(190.0,80.0,"Curl tau annual mean [x 1e-7 N/m3] (SCOW)",60,0,0.10)
'run cbarn2.gs 0.8 0 4.25 0.5 0 2'
*-------------------------------
'print'
'disable print'
*-------------------------------
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
