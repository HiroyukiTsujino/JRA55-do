function vgeo(args)
say '1 .. shade or line    '
say '2,3,4,5 lat_min, lat_max, z_min, z_max, can be omitted'
*
*
*
*if (subwrd(args,2) ='')
  'q dims'
  xdim = sublin(result,2)
  ydim = sublin(result,3)
  zdim = sublin(result,4)
  jy = subwrd(ydim,9)
  x0 = subwrd(xdim,6)
  x1 = subwrd(xdim,8)
  z0 = subwrd(zdim,6)
  z1 = subwrd(zdim,8)
  oriline = sublin(result,1)
  num_orgin = subwrd(oriline,5)
  say x0 x1
  say z0 z1
  say jy
*else
*  x0 = subwrd(args,2)
*  x1 = subwrd(args,3)
*  z0 = subwrd(args,4)
*  z1 = subwrd(args,5)
*endif
*
* model geometry function
*
'open geoall'
valname = 'hgeometry'
*
* identify what number is the geo file
*
'query files'
linenum = 1
iden1 = sublin(result,linenum)
iden2 = subwrd(iden1,4)
num_vgeo   = subwrd(iden1,2)
while ( iden2 != valname )
  linenum = linenum + 3 
  iden1 = sublin(result,linenum)
  iden2 = subwrd(iden1,4)
  num_vgeo = subwrd(iden1,2)
endwhile
*
*  dimension of the model
*
'set dfile 'num_vgeo
'query file'
dimline = sublin(result,5)
nx = subwrd(dimline,3)
nz = subwrd(dimline,9)
*
* identify what  number is the HISTORY5 file
*
'open HISTORY5'       
'query files'
linenum = 1
iden1 = sublin(result,linenum)
iden2 = subwrd(iden1,4)
num_Mover = subwrd(result,2)
while ( iden2 != Mover )
  linenum = linenum + 3 
  iden1 = sublin(result,linenum)
  iden2 = subwrd(iden1,4)
  num_Mover = subwrd(iden1,2)
endwhile
*
*
*
'set dfile 'num_vgeo
geo = 'geoall'
*
*     shade type land drawing
*
if(subwrd(args,1) = shade)
  'set gxout fgrid'
  'set fgvals 0 1'
  'set y 'jy
  'set lon 'x0' 'x1
  'set lev 'z0' 'z1
  'set yflip on'
  'd 'geo
endif
*
*
*    line type land drawing
*
'set dfile 'num_vgeo
if ( subwrd(args,1) = line )
  'set x 'ix
  'set lat 'y0' 'y1
  'set lev 'z0' 'z1
  'd 'geo         
*
*     dummy
*
  'set grads off'
  'set yflip on'
  'set gxout fgrid'
  'set fgvals 0 0'
  'd 'geo
*
*
  k = 1
  'set z 1'
  while ( k <= nz )
    j = 1
    while ( j <= ny-1 & k != nz)
      'set x 'ix
      'set z 'k
      'set y 'j
      'd 'geo
      geo0 = subwrd(result,4)
      'set y 'j+1
      'd 'geo
      geo1 = subwrd(result,4)
      'set y 'j
      'set z 'k+1
      'd 'geo
      geo2 = subwrd(result,4)
      if ( geo0 + geo1 = 1 )
        'set dfile 'num_Mover
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'set y 'j+1
        yy1 = subwrd(result,4)
        'set z 'k' 'k+1
        zz1 = subwrd(result,4)
        zz2 = subwrd(result,5)
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'run drawline 'yy1' 'zz1' 'yy1' 'zz2' 'y0' 'y1' 'z1' 'z0
        'set dfile 'num_vgeo
      endif
      if ( geo0 + geo2 = 1 ) 
        'set dfile 'num_Mover
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'set y 'j' 'j+1
        yy1 = subwrd(result,4)
        yy2 = subwrd(result,5)
        'set z 'k+1
        zz1 = subwrd(result,4)
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'run drawline 'yy1' 'zz1' 'yy2' 'zz1' 'y0' 'y1' 'z1' 'z0
        'set dfile 'num_vgeo
      endif
      j = j + 1
    endwhile
    while ( j <= ny-1 & k = nz)
      'set x 'ix
      'set z 'k
      'set y 'j
      'd 'geo
      geo0 = subwrd(result,4)
      'set y 'j+1
      'd 'geo
      geo1 = subwrd(result,4)
      if ( geo0 + geo1 = 1 )
        'set dfile 'num_Mover
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'set y 'j+1
        yy1 = subwrd(result,4)
        'set z 'k' 'k+1
        zz1 = subwrd(result,4)
        zz2 = subwrd(result,5)
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'run drawline 'yy1' 'zz1' 'yy1' 'zz2' 'y0' 'y1' 'z1' 'z0
        'set dfile 'num_vgeo
      endif
      j = j + 1
    endwhile
    if ( k <= nz-1)
      j = ny
      'set x 'ix
      'set z 'k
      'set y 'j
      'd 'geo
      geo0 = subwrd(result,4)
      'set z 'k+1
      'd 'geo
      geo1 = subwrd(result,4)
      if ( geo0 + geo1 = 1 )
        'set dfile 'num_Mover
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'set y 'j' 'j+1
        yy1 = subwrd(result,4)
        yy2 = subwrd(result,5)
        'set z 'k+1
        zz1 = subwrd(result,4)
        'set x 'ix
        'set lat 'y0' 'y1
        'set lev 'z0' 'z1
        'set yflip on'
        'run drawline 'yy1' 'zz1' 'yy2' 'zz1' 'y0' 'y1' 'z1' 'z0
        'set dfile 'num_vgeo
      endif
    endif
    k = k + 1
  endwhile
endif
'set dfile 'num_orgin
'set x 'ix
'set lat 'y0' 'y1
'set lev 'z0' 'z1
'set yflip on'
return




