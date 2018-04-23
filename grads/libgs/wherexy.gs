say 'Click near bottom of the screen to quit'
while (1)
      'query bpos'
      x = subwrd(result,3)
      y = subwrd(result,4)
      if ( y<'0.5'); break; endif;
      'query xy2gr 'x ' 'y
      x1 = subwrd(result,3)
      y1 = subwrd(result,6)
      x2 = mkint( x1 )
      y2 = mkint( y1 )
      x2 = cyclic( x2 )
      say 'X = 'x1 ' Y = 'y1
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

