*-------------------------------------------
* GrADs script
*     Comparison of flux among products
*-------------------------------------------
*
function check(args)

reinit

mnyr1=subwrd(args,1)
mnyr2=subwrd(args,2)

*===========================================
region.1=arctic;lonbase.1=200;latbase.1=80
region.2=subpnatl;lonbase.2=320;latbase.2=52
region.3=subanatl;lonbase.3=290;latbase.3=41
region.4=subtnatl;lonbase.4=305;latbase.4=30
region.5=tropnatl;lonbase.5=320;latbase.5=18
region.6=tropsatl;lonbase.6=330;latbase.6=0
region.7=subtsatl;lonbase.7=345;latbase.7=-17
region.8=subpsatl;lonbase.8=330;latbase.8=-40
region.9=tropind;lonbase.9=65;latbase.9=-5
region.10=mediterranean;lonbase.10=363;latbase.10=35
region.11=weddell;lonbase.11=317;latbase.11=-70
region.12=soatl;lonbase.12=365;latbase.12=-60
region.13=troppac;lonbase.13=200;latbase.13=-5
region.14=subtsind;lonbase.14=70;latbase.14=-25
region.15=soind;lonbase.15=70;latbase.15=-55
region.16=subtnpac;lonbase.16=170;latbase.16=35
region.17=tropnpac;lonbase.17=170;latbase.17=17
region.18=subtspac;lonbase.18=220;latbase.18=-35
region.19=sopac;lonbase.19=210;latbase.19=-60
region.20=subpnpac;lonbase.20=175;latbase.20=55
region.21=black;lonbase.21=40;latbase.21=42
*===========================================
*-- draw boundary
'open region_index_fulldiv.ctl'

'set vpage 0.0 8.5 0.0 11.0'
'set parea 0.5 8.0 0.5 10.5'

'set gxout grfill'
'set mproj scaled'
'set map 15 1 3'
'set lat -90 90'
'set lon 20 400'
'set t 1'
'set grads off'
'set xlopts 1 3 0.12'
'set ylopts 1 3 0.12'
'set xlint 60'
'set ylint 20'
'set clevs 0.25'
'set rgb 101 180 180 180'
'set ccols 0 101'
'd sqrt(cdiff(index,x)*cdiff(index,x) + cdiff(index,y)*cdiff(index,y))'
*
*--------------------------------------------------------------
* scale quantity for 1 degree latitude
_scalefc=2.0 
* unit scale of axis
_unit=10.0
*--------------------------------------------------------------

'set line 0'
'drawrecf 55 45 130 65'
'set line 1'
'drawrec  55 45 130 65'

lon0=80
lat0=55
size=8.0
dlon=8
lon1=lon0
lat1=lat0

lon1=lon1+dlon
rc = drawLineMark( lon1, lat1, -size, 4, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 2, 0, 8, 0.1)
lon1=lon1+dlon
rc = drawLineMark( lon1, lat1, -size, 4, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 2, 0, 8, 0.1)
lon1=lon1+dlon
rc = drawLineMark( lon1, lat1, -size, 4, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 2, 0, 8, 0.1)
lon1=lon1+dlon
rc = drawLineMark( lon1, lat1, -size, 4, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 2, 0, 8, 0.1)

ma=1
mi=1
size=5.0
'run myaxis_w 'lon0' 'lat0' 90 'size' 0.1 1 'ma' 'mi' 0.1 '_unit' 1' 

'set line 1'
'set string 1 c 1 0'
'set strsiz 0.045'
*
dlon=8
dlat=6.5
dlat2=5
*
lon1=lon0
lat1=lat0
*
lon1=lon1+dlon
'dstring 'lon1' 'lat1+dlat' SW'
lon1=lon1+dlon
'dstring 'lon1' 'lat1+dlat' LW'
lon1=lon1+dlon
'dstring 'lon1' 'lat1+dlat' LAT'
lon1=lon1+dlon
'dstring 'lon1' 'lat1+dlat' SEN'
'set strsiz 0.06'
lon1=lon1+dlon
lat2=lat0-dlat2
'dstring 'lon1' 'lat2' raw'
lat2=lat0+dlat2
'dstring 'lon1' 'lat2' do'

lon2=lon0-dlon*2
lat2=lat0
'set strsiz 0.06'
'set string 1 c 1 90'
'dstring 'lon2' 'lat2' W/m2'
'set string 1 c 1 0'

*--------------------------------------------------------------
i=1
*--------------------------------------------------------------
while(i<21)
**************************************if (i!=10)
'open nswrf-raw-v1_0-'region.i'.ctl'
'open nswrf-do-v1_2-'region.i'.ctl'
'open nswrf-core-'region.i'.ctl'

'open nlwrf-raw-v1_0-'region.i'.ctl'
'open nlwrf-do-v1_2-'region.i'.ctl'
'open nlwrf-core-'region.i'.ctl'

'open latent-raw-v1_0-'region.i'.ctl'
'open latent-do-v1_2-'region.i'.ctl'
'open latent-core-'region.i'.ctl'

'open sensible-raw-v1_0-'region.i'.ctl'
'open sensible-do-v1_2-'region.i'.ctl'
'open sensible-core-'region.i'.ctl'
*
'set dfile 2'
*--
'set x 1'
'set y 1'
'set z 1'
'set time 1'mnyr1 ' 1'mnyr2
'q dims'
line1=sublin(result,5)
*--
if ( mnyr1 = mnyr2 ) 
  t1=subwrd(line1,9)
  t2=t1
else
  t1=subwrd(line1,11)
  t2=subwrd(line1,13)
  mn1=substr(mnyr1,1,3)
  mn2=substr(mnyr2,1,3)
  yr1=substr(mnyr1,4,7)
  yr2=substr(mnyr2,4,7)
endif
*--
rc = writebars(t1, t2, lonbase.i, latbase.i, 0, i)
*
'close 13'
'close 12'
'close 11'
'close 10'
'close 9'
'close 8'
'close 7'
'close 6'
'close 5'
'close 4'
'close 3'
'close 2'
**************************************endif
i=i+1
endwhile
*--------------------------------------------------------------
'close 1'
'enable print hflux.gm'
'print'
'disable print'
'!gxeps -c -i hflux.gm -o hflux_region.eps'
'!rm hflux.gm'
*
*==============================================================
function writebars  ( argv1, argv2, argv3, argv4, argv5, argv6 )
*--------------------------------------------------------------

t1=subwrd(argv1,1)
t2=subwrd(argv2,1)
lon1=subwrd(argv3,1)
lat1=subwrd(argv4,1)
opt1=subwrd(argv5,1)
rgn_num=subwrd(argv6,1)

lat0=lat1
dlat=2
dlon=8

lon1_org = lon1
lat1_org = lat1

lon1=lon1+dlon
rc = setAREAnum (t1, t2, 2, 4)
rc = drawLineMark( lon1, lat1, _finvdif, 4, opt1, 6, 0.1)
rc = setAREAnum (t1, t2, 3, 4)
rc = drawLineMark( lon1, lat1, _finvdif, 2, opt1, 8, 0.1)

lon1=lon1+dlon
rc = setAREAnum (t1, t2, 5, 7)
rc = drawLineMark( lon1, lat1, _finvdif, 4, opt1, 6, 0.1)
rc = setAREAnum (t1, t2, 6, 7)
rc = drawLineMark( lon1, lat1, _finvdif, 2, opt1, 8, 0.1)

lon1=lon1+dlon
rc = setAREAnum (t1, t2, 8, 10)
rc = drawLineMark( lon1, lat1, _finvdif, 4, opt1, 6, 0.1)
rc = setAREAnum (t1, t2, 9, 10)
rc = drawLineMark( lon1, lat1, _finvdif, 2, opt1, 8, 0.1)

lon1=lon1+dlon
rc = setAREAnum (t1, t2, 11, 13)
rc = drawLineMark( lon1, lat1, _finvdif, 4, opt1, 6, 0.1)
rc = setAREAnum (t1, t2, 12, 13)
rc = drawLineMark( lon1, lat1, _finvdif, 2, opt1, 8, 0.1)

'set strsiz 0.06'
lon1=lon1+dlon*0.75
lat2=lat0+dlat
'dstring 'lon1' 'lat2' ' rgn_num


_maxvalue_finput=10
_minvalue_finput=-10
lat0=lat1_org
size=_unit/_scalefc
lon0=lon1_org
ma=math_nint(_maxvalue_finput/_unit)
mi=-math_nint(_minvalue_finput/_unit)

say size
say ma
say mi

'run myaxis_w 'lon0' 'lat0' 90 'size' 0.1 1 'ma' 'mi' 0.1 '_unit' 1' 

return

*=================================================
function setAREAnum ( argv1, argv2, argv3, argv4 )
*-------------------------------------------------
t1=subwrd(argv1,1)
t2=subwrd(argv2,1)
ftrg=subwrd(argv3,1)
fref=subwrd(argv4,1)

*say t1
*say t2

'd ave(f.'ftrg'-f.'fref',t='t1',t='t2')'
*
say result
line2=sublin(result,2)
aa=subwrd(line2,4)
if ( aa <= -999 )
  aa=0
endif

_finput = aa
_finvdif = aa

if ( _finput > _maxvalue_finput )
  _maxvalue_finput = _finput
endif
if ( _finput < _minvalue_finput )
  _minvalue_finput = _finput
endif


return

*========================================================
function drawArrow ( argv1, argv2, argv3, argv4, argv5 )
*--------------------------------------------------------
*
length = 0.3
minscale=0.01
sizehead=0
width = 0.075
*
x1 = argv1
y1 = argv2
value = argv3
colorar = argv4
opt = argv5
*
length = value/_scalefc
*
if ( opt = 1 ) 
    angle=0
    'arshade 'x1' 'y1' 'angle' 'length' 'width' 'sizehead' 'colorar
else
    angle=90
    'arshade 'x1' 'y1' 'angle' 'length' 'width' 'sizehead' 'colorar
endif
return

*========================================================
function drawLineMark ( argv1, argv2, argv3, argv4, argv5, argv6, argv7 )
*--------------------------------------------------------
*
length = 0.3
minscale=0.01
sizehead=0
width = 0.075
*
x1      = argv1
y1      = argv2
value   = argv3
colorar = argv4
opt     = argv5
mrk     = argv6
siz     = argv7
*
length = value/_scalefc
say length
*
if ( opt = 1 ) 
    'drawline 'x1' 'y1' 'x1+length' 'y1
    'drawmark 'mrk' 'x1+length' 'y1' 'siz' 'colorar' 1'
else
    'drawline 'x1' 'y1' 'x1' 'y1+length
    'drawmark 'mrk' 'x1' 'y1+length' 'siz' 'colorar' 1'
endif
return
