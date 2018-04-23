say 'Click near bottom of the screen to quit'
while (1)
      'query bpos'
      x = subwrd(result,3)
      y = subwrd(result,4)
      if ( y<'0.5'); break; endif;
      'query xy2w 'x ' 'y
      Lon = subwrd(result,3)
      Lat = subwrd(result,6)
      if (Lon < '0.0')
          Lon = Lon + 360.0
      endif
      x1 = Lon 
      y1 = Lat + 90
      say 'Lon = 'Lon ' Lat = 'Lat ' X = 'x1 ' Y = 'y1
endwhile
