*-------------------------------------------
* GrADs script
*     Comparison of flux among products
*-------------------------------------------
*
function check(args)

reinit

mnyr1=subwrd(args,1)
mnyr2=subwrd(args,2)
rgn_num=subwrd(args,3)

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
region.10=noname
region.11=weddell;lonbase.11=317;latbase.11=-70
region.12=soatl;lonbase.12=360;latbase.12=-60
region.13=troppac;lonbase.13=200;latbase.13=-5
region.14=subtsind;lonbase.14=70;latbase.14=-25
region.15=soind;lonbase.15=70;latbase.15=-55
region.16=subtnpac;lonbase.16=170;latbase.16=35
region.17=tropnpac;lonbase.17=170;latbase.17=17
region.18=subtspac;lonbase.18=220;latbase.18=-35
region.19=sopac;lonbase.19=210;latbase.19=-60
region.20=subpnpac;lonbase.20=175;latbase.20=55
region.21=mediterranean;lonbase.21=363;latbase.21=35
region.22=black;lonbase.22=40;latbase.22=42
*===========================================
rarea.1=1.61729e4
rarea.2=7.71401e3
rarea.3=6.01118e3
rarea.4=8.88356e3
rarea.5=1.29335e4
rarea.6=1.35063e4
rarea.7=7.44091e3
rarea.8=2.27256e4
rarea.9=2.89644e4
rarea.11=5.67875e3
rarea.12=5.38855e3
rarea.13=7.17234e4
rarea.14=1.25895e4
rarea.15=2.89825e4
rarea.16=2.56509e4
rarea.17=2.60758e4
rarea.18=2.05813e4
rarea.19=3.20256e4
rarea.20=9.16571e3
rarea.21=2.71503e3
rarea.22=4.84307e2
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
_scalefc=1.0
* unit scale of axis
_unit=5.0
*--------------------------------------------------------------

'set line 0'
'drawrecf 55 45 130 65'
'set line 1'
'drawrec  55 45 130 65'

lon0=80
lat0=55
size=5
dlon=8
lon1=lon0
lat1=lat0

lon1=lon1+dlon
rc = drawLineMark( lon1, lat1, -size, 2, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 4, 0, 8, 0.1)
lon1=lon1+dlon
rc = drawLineMark( lon1, lat1, -size, 2, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 4, 0, 8, 0.1)
lon1=lon1+dlon
*rc = drawLineMark( lon1, lat1,-size, 2, 0, 6, 0.1)
rc = drawLineMark( lon1, lat1, size, 4, 0, 8, 0.1)

ma=1
mi=1
size=5
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
'dstring 'lon1' 'lat1+dlat' E'
lon1=lon1+dlon
'dstring 'lon1' 'lat1+dlat' P'
lon1=lon1+dlon
'dstring 'lon1' 'lat1+dlat' Rx10'
*
'set strsiz 0.06'
lon1=lon1+dlon
lat2=lat0-dlat2
'dstring 'lon1' 'lat2' raw'
lat2=lat0+dlat2
'dstring 'lon1' 'lat2' do'

lon2=lon0-dlon*2.5
lat2=lat0
'set strsiz 0.07'
'set string 1 c 1 90'
'dstring 'lon2' 'lat2' mg/m2/sec'
'set string 1 c 1 0'


*--------------------------------------------------------------
i=rgn_num
*--------------------------------------------------------------
if (i!=10)

'open evapor-raw-v1_0-'region.i'.ctl'
'open evapor-do-v1_2-'region.i'.ctl'
'open evapor-core-'region.i'.ctl'

'open precip-raw-v1_0-'region.i'.ctl'
'open precip-do-v1_2-'region.i'.ctl'
'open precip-core-'region.i'.ctl'

'open runoff_all-do-v1_1-'region.i'.ctl'
'open runoff_all-core-'region.i'.ctl'

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
say ' '
say 'region = ' i ' ' region.i

rc = writebars(t1, t2, lonbase.i, latbase.i, rarea.i, 0)
*

'close 9'
'close 8'
'close 7'
'close 6'
'close 5'
'close 4'
'close 3'
'close 2'
else
  say 'region 'i' is not defined'
endif
*--------------------------------------------------------------
'close 1'
*==============================================================
function writebars  ( argv1, argv2, argv3, argv4, argv5, argv6 )
*--------------------------------------------------------------

t1=subwrd(argv1,1)
t2=subwrd(argv2,1)
lon1=subwrd(argv3,1)
lat1=subwrd(argv4,1)
area=subwrd(argv5,1)
opt1=subwrd(argv6,1)

lat0=lat1
dlat=2
dlon=8

lon1_org = lon1
lat1_org = lat1

lon1=lon1+dlon

say ' '
say 'Total'
'd ave(-f.3+f.6+f.8,t='t1',t='t2')'
line2=sublin(result,2)
aa=subwrd(line2,4)
say '  JRA55-do   'aa * 1e6
'd ave(-f.4+f.7+f.9,t='t1',t='t2')'
line2=sublin(result,2)
aa=subwrd(line2,4)
say '  CORE       'aa * 1e6


say ' '
say 'Evaporation'
target="JRA55-raw"
rc = setAREAnum (t1, t2, 2, 4, target)
*rc = drawLineMark( lon1, lat1, _finvdif*area, 2, opt1, 6, 0.1)
rc = drawLineMark( lon1, lat1, _finvdif, 2, opt1, 6, 0.1)

target="JRA55-do"
rc = setAREAnum (t1, t2, 3, 4, target)
*rc = drawLineMark( lon1, lat1, _finvdif*area, 4, opt1, 8, 0.1)
rc = drawLineMark( lon1, lat1, _finvdif, 4, opt1, 8, 0.1)

lon1=lon1+dlon
say ' '
say 'Precipitation'
target="JRA55-raw"
rc = setAREAnum (t1, t2, 5, 7, target)
*rc = drawLineMark( lon1, lat1, _finvdif*area, 2, opt1, 6, 0.1)
rc = drawLineMark( lon1, lat1, _finvdif, 2, opt1, 6, 0.1)
target="JRA55-do"
rc = setAREAnum (t1, t2, 6, 7, target)
*rc = drawLineMark( lon1, lat1, _finvdif*area, 4, opt1, 8, 0.1)
rc = drawLineMark( lon1, lat1, _finvdif, 4, opt1, 8, 0.1)

say ' '
say 'Runoff'
target="JRA55-do"
lon1=lon1+dlon
rc = setAREAnum (t1, t2, 8, 9, target)
*rc = drawLineMark( lon1, lat1, _finvdif*area*10, 4, opt1, 8, 0.1)
rc = drawLineMark( lon1, lat1, _finvdif*10, 4, opt1, 8, 0.1)

_maxvalue_finput=5
_minvalue_finput=-5
lat0=lat1_org
size=_unit/_scalefc
lon0=lon1_org
ma=math_nint(_maxvalue_finput/_unit)
mi=-math_nint(_minvalue_finput/_unit)

*say size
*say ma
*say mi

'run myaxis_w 'lon0' 'lat0' 90 'size' 0.1 1 'ma' 'mi' 0.1 '_unit' 1' 

return

*=================================================
function setAREAnum ( argv1, argv2, argv3, argv4, argv5 )
*-------------------------------------------------
t1=subwrd(argv1,1)
t2=subwrd(argv2,1)
ftrg=subwrd(argv3,1)
fref=subwrd(argv4,1)
targname=subwrd(argv5,1)

*say t1
*say t2

'd ave(f.'ftrg',t='t1',t='t2')'
line2=sublin(result,2)
aa=subwrd(line2,4)
say '  'targname '   'aa * 1e6

'd ave(f.'fref',t='t1',t='t2')'
line2=sublin(result,2)
aa=subwrd(line2,4)
say '  ref (CORE)  'aa * 1e6

'd ave(f.'ftrg'-f.'fref',t='t1',t='t2')'
*
*say result
line2=sublin(result,2)
aa=subwrd(line2,4)
if ( aa <= -999 )
  aa=0
endif

_finput = aa * 1e6
_finvdif = aa * 1e6

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
*
if ( opt = 1 ) 
    'drawline 'x1' 'y1' 'x1+length' 'y1
    'drawmark 'mrk' 'x1+length' 'y1' 'siz' 'colorar' 1'
else
    'drawline 'x1' 'y1' 'x1' 'y1+length
    'drawmark 'mrk' 'x1' 'y1+length' 'siz' 'colorar' 1'
endif
return
