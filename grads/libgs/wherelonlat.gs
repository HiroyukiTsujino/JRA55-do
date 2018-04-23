function wherelonlat (args)
arg1=subwrd(args,1)
say arg1
say 'Click near bottom of the screen to quit'
'!rm -f path.txt'
while (1)
      'query bpos'
      x = subwrd(result,3)
      y = subwrd(result,4)
      if ( y<'0.5'); break; endif;
      'query xy2gr 'x ' 'y
      x1 = subwrd(result,3)
      y1 = subwrd(result,6)
      x2 = math_int( x1 )
      y2 = math_int( y1 )
*      x2 = mkint( x1 )
*      y2 = mkint( y1 )
*      x2 = cyclic( x2 )
      'set x 'x2
      lon2=subwrd(result,4)
      'set y 'y2
      lat2=subwrd(result,4)
*      say 'X = 'x2 ' Y = 'y2  
*      say 'lon = 'lon2 ' lat = 'lat2
      if (arg1 = 'west' ) 
        lon3 = - (360 - lon2)
        say lon3 ' 'lat2
        reclog=lon3 ' 'lat2
        rc = write( path.txt, reclog, append)     
*      endif
*      if (arg1 = 'east' ) 
*        lon3 = - (360 - lon2)
*        say lon2 ' 'lat2
*      endif
      else
        say lon2 ' 'lat2
        reclog=lon2 ' 'lat2
        rc = write( path.txt, reclog, append)     
      endif
endwhile

function mkint ( i )
j = -1500
i = i - 0.5
while ( j < 4000)
   j = j + 1
   if ( i <= j)
      return j
   endif
endwhile

function cyclic ( i )
  'q file'
*  say result
  line = sublin(result,5)
  nx   = subwrd(line,3)
  if ( i <= 0 ) 
      i = nx + i
  endif
  return i
end


