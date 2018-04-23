function area (args)
 name1 = subwrd(args,1)
 name2 = subwrd(args,2)
 xlength  = subwrd(args,3)
 ylength  = subwrd(args,4)
 xshift   = subwrd(args,5)
 yshift   = subwrd(args,6)
 if ( name1 ='help')
    say 'type name lon_length lat_length lon_shift lat_shift'
    return
 endif
*
 rc = location ( name1, name2 )
 if (xlength ='') 
    xlength = 0
    ylength = 0
 endif
 if (xshift = '')
    xshift = 0
    yshift = 0
 endif
 rc = getrange ( xlength, ylength, xshift, yshift )
 'set lon '_x1' '_x2
 line = sublin(result,1)
 say line
 'set lat '_y1' '_y2
 line = sublin(result,1)
 say line
return

function location (name1, name2)
if (name1 ='')
  say 'cape'
  say 'island'
  say 'sea'
  say 'strait'
  return
endif
*
* Cape
if (name1 = 'cape')
   if (name2 ='')
     say 'Inubou-saki'
     say 'Shiono-misaki'
   endif
   if (name2 = 'Inubou-saki')
     _x0 =  140.53
     _y0 =   35.42
   endif
   if (name2 = 'Shiono-misaki')
     _x0 =  135.45
     _y0 =   33.26
   endif
endif
*
* Island
if (name1 = 'island')
  if (name2 ='')
    say 'Iceland'
  endif
   if (name2 = 'Iceland')
     _x0 =  -18.00
     _y0 =   65.00
   endif
endif
*
* Strait
if (name1 = 'strait')
  if (name2 ='')
    say 'Gibraltar'
  endif
  if (name2 = 'Gibraltar')
     _x0 =   -5.21
     _y0 =   36.08
  endif
  return
endif
return

function getrange(xlength, ylength, xshift, yshift)
 _x1 = _x0 - 0.5 * xlength + xshift
 _x2 = _x0 + 0.5 * xlength + xshift
 _y1 = _y0 - 0.5 * ylength + yshift
 _y2 = _y0 + 0.5 * ylength + yshift
return





