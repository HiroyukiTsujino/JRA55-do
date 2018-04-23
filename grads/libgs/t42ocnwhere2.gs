say 'Click near bottom of the screen to quit'
while (1)
      'query bpos'
      x = subwrd(result,3)
      y = subwrd(result,4)
      if ( y<'0.5'); break; endif;
      'query xy2gr 'x ' 'y
      x1 = subwrd(result,3)
      y1 = subwrd(result,6)
      x2 = mkint( x1  )
      y2 = mkint( y1 )
      y3 = (y2 -1 ) 
      say 'x = 'x2-1 ' y = 'y3
endwhile

function mkint ( i )
j = 0
i = i + 0.5
while ( j < 300)
   j = j + 1
   if ( i <= j)
      return j
   endif
endwhile


