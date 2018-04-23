function hgeo(args)
say '1 .. shade or line    '
say '2 .. vetical level, can be omitted   '
say '3,4,5,6   lon_min, lon_max, lat_min, lat_max, can be omitted'
*
*
if (subwrd(args,3) = '')
  'q dims'
  xdim = sublin(result,2)
  ydim = sublin(result,3)
  x0 = subwrd(xdim,6)
  x1 = subwrd(xdim,8)
  y0 = subwrd(ydim,6)
  y1 = subwrd(ydim,8)
else
  x0 = subwrd(args,3)
  x1 = subwrd(args,4)
  y0 = subwrd(args,5)
  y1 = subwrd(args,6)
endif
if (subwrd(args,2) = '')
  zdim = sublin(result,4)
  zz = subwrd(zdim,9)
  oriline = sublin(result,1)
  num_orgin = subwrd(oriline,5)
else
  zz = subwrd(args,2)
endif
'q gxinfo'
grline = sublin(result,1)
graphic_orgin = subwrd(grline,4)
*
*
* model geometry function
*
'set mpdraw off'
'set grads off'
'open geoall'
*
* identify what number is the geo file
*
'query files'
linenum = 1
iden1 = sublin(result,linenum)
iden2 = subwrd(iden1,4)
num_hgeo   = subwrd(iden1,2)
while ( iden2 != 'hgeometry' )
  linenum = linenum + 3 
  iden1 = sublin(result,linenum)
  iden2 = subwrd(iden1,4)
  num_hgeo = subwrd(iden1,2)
endwhile
*
*  dimension of the model
*
'set dfile 'num_hgeo
'query file'
dimline = sublin(result,5)
nx = subwrd(dimline,3)
ny = subwrd(dimline,6)
*
* identify what  number is the HISTORY1 file
*
'open HISTORY1'       
'query files'
linenum = 1
     
iden1 = sublin(result,linenum)
iden2 = subwrd(iden1,4)
num_UV = subwrd(result,2)
while ( iden2 != UV )
  linenum = linenum + 3 
  iden1 = sublin(result,linenum)
  iden2 = subwrd(iden1,4)
   num_UV = subwrd(iden1,2)
endwhile
*
*     
*     

*
*
*
*    shade type land drawing
*
'set dfile 'num_hgeo

if(subwrd(args,1) = shade)
  'set gxout fgrid'
  'set fgvals 0 1'
  'set lon 'x0' 'x1
  'set lat 'y0' 'y1
  'set z 'zz
  'set t 1'
  'd geoall'
endif
*
*
*    line type land drawing
*
'set dfile 'num_hgeo
if ( subwrd(args,1) = line )
  'set lon 'x0' 'x1
  'set lat 'y0' 'y1
  'set z 'zz
  'set t 1'
*
*     dummy
*
  'set gxout fgrid'
  'set fgvals 0 0'
  'd 'geoall
*
*
  j = 2
  while ( j <= ny-1 )
    i = 1
    while ( i <= nx-1 )
      'set x 'i
      'set y 'j
      'd 'geoall
      geo0 = subwrd(result,4)
      'set x 'i+1
      'd 'geoall
      geo1 = subwrd(result,4)
      'set x 'i
      'set y 'j+1
      'd 'geoall
      geo2 = subwrd(result,4)
      if ( geo0 + geo1 = 1 )
        'set dfile 'num_UV
        'set z 'zz
        'set lon 'x0' 'x1
        'set lat 'y0' 'y1
        'set x 'i
        xx1 = subwrd(result,4)
        if (xx1 > x1) 
           xx1 = xx1 - 360
        endif
        'set y 'j-1' 'j
        yy1 = subwrd(result,4)
        yy2 = subwrd(result,5)
        'set z 'zz
        'set lon 'x0' 'x1
        'set lat 'y0' 'y1
        'run drawline 'xx1' 'yy1' 'xx1' 'yy2' 'x0' 'x1' 'y0' 'y1
        'set dfile 'num_hgeo
      endif
      if ( geo0 + geo2 = 1 ) 
        'set dfile 'num_UV
        'set z 'zz
        'set lon 'x0' 'x1
        'set lat 'y0' 'y1
        'set x 'i-1' 'i
        xx1 = subwrd(result,4)
        xx2 = subwrd(result,5)
        if( xx1 >= x1 )
           xx1 = xx1 - 360
        endif
        if( xx2 > x1 )
           xx2 = xx2 - 360
        endif
        'set y 'j
        yy1 = subwrd(result,4)
        'set z 'zz
        'set lon 'x0' 'x1
        'set lat 'y0' 'y1
        'set yflip on'
        if (xx2 < xx1 ) 
          'run drawline 'xx1' 'yy1' 'x1 ' 'yy1' 'x0' 'x1' 'y0' 'y1         
          'run drawline 'x0 ' 'yy1' 'xx2' 'yy1' 'x0' 'x1' 'y0' 'y1         
        else
          'run drawline 'xx1' 'yy1' 'xx2' 'yy1' 'x0' 'x1' 'y0' 'y1
        endif
        'set dfile 'num_hgeo
      endif
      i = i + 1
    endwhile
    'set z 'zz
    'set x 'nx
    'set y 'j
    'd 'geoall
    geo0 = subwrd(result,4)
    'set x '1
    'd 'geoall
    geo1 = subwrd(result,4)
    'set x 'nx
    'set y 'j+1
    'd 'geoall
    geo2 = subwrd(result,4)
    if ( geo0 + geo1 = 1 )
      'set dfile 'num_UV
      'set z 'zz
      'set x 'nx
      xx1 = subwrd(result,4)
      if ( xx1 > x1 )
        xx1 = xx1 - 360
      endif
      'set y 'j-1' 'j
       yy1 = subwrd(result,4)
       yy2 = subwrd(result,5)
      'set z 'zz
      'set lon 'x0' 'x1
      'set lat 'y0' 'y1
      'run drawline 'xx1' 'yy1' 'xx1' 'yy2' 'x0' 'x1' 'y0' 'y1
      'set dfile 'num_hgeo
    endif
    if ( geo0 + geo2 = 1 )
      'set dfile 'num_UV
      'set z 'zz
      'set x 'nx-1' 'nx
      xx1 = subwrd(result,4)
      xx2 = subwrd(result,5)
      if ( xx1 > x1 )
        xx1 = xx1 - 360
      endif
      if ( xx2 >= x1 )
        xx2 = xx2 - 360
      endif
      'set x 'nx
      'set y 'j
      yy1 = subwrd(result,4)
      'set z 'zz
      'set lon 'x0' 'x1
      'set lat 'y0' 'y1
      if (xx2 < xx1 ) 
        'run drawline 'xx1' 'yy1' 'x1 ' 'yy1' 'x0' 'x1' 'y0' 'y1         
        'run drawline 'x0 ' 'yy1' 'xx2' 'yy1' 'x0' 'x1' 'y0' 'y1         
      else
        'run drawline 'xx1' 'yy1' 'xx2' 'yy1' 'x0' 'x1' 'y0' 'y1
      endif
      'set dfile 'num_hgeo
    endif
    j = j + 1
  endwhile
endif
*
*
'set dfile 'num_orgin
'set lon 'x0' 'x1
'set lat 'y0' 'y1
'set gxout 'graphic_orgin
*
return




