*-------------------------------
function main(args)
*-------------------------------
month=subwrd(args,1)
version=subwrd(args,2)
if(month="")
 say ' Usage: curl_tau_EastPac #month #version'
 exit
endif
if(month<9)
 corrector=12
else
 corrector=0
endif
tstr=month + corrector
tend=123
say ' Forcing: 'type
say '    tstr: 'tstr
say '    tend: 'tend
*-------------------------------
if(month<10)
 zero="0"
else
 zero=""
endif
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
fileo="curl_EastPac_"zero month"_"version".eps"
*-------------------------------
'reinit'
'enable print 'filem
*-------------------------------
'set vpage 0.0 8.5 0.0 11.0'
'sdfopen /work116/htsujino/SCOW/wind_stress_curl_monthly_maps.nc'
'open hcurl_'version'.ctl'
'set lon 190 300'
'set lat -25 25'
'set xlint 20'
'set ylint 10'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'
*-------------------------------
'set parea 1.0 8.0 6.0 10.0'
'run colmap.gs rb26'
'set clevs -5.0 -4.0 -3.0 -2.0 -1.5 -1.0 -0.7 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.7 1.0 1.5 2.0 3.0 4.0 5.0'
'set grads off'
'set gxout grfill'
'set dfile 2'
'd ave(curl*1e7,t='tstr',t='tend',12)'
cmon=substr(varscow.month,1,3)
label(245.0,27.0,"Curl tau in "cmon". [x 1e-7 N/m] ("version") [Sep.1999-Oct.2009]",60,0,0.10)
*-------------------------------
'set parea 1.0 8.0 1.2 5.2'
'run colmap.gs rb26'
'set clevs -5.0 -4.0 -3.0 -2.0 -1.5 -1.0 -0.7 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.7 1.0 1.5 2.0 3.0 4.0 5.0'
'set grads off'
'set gxout grfill'
'set dfile 1'
'set t 1'
'd maskout('varscow.month','varscow.month'+9000)'
label(245.0,27.0,"Curl tau in "cmon". [x 1e-7 N/m] (SCOW) [Sep.1999-Oct.2009]",60,0,0.10)
'run cbarn2.gs 0.8 0 4.25 0.5 0 2'
*-------------------------------
'print'
'disable print'
*-------------------------------
'!gxeps -c -i 'filem' -o 'fileo
'!rm -f 'filem
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
