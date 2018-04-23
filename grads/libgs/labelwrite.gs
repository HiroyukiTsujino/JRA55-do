'set string 1 c 3'
'set strsiz 0.06 0.07'
while (1)
  prompt 'label: '
  pull lab
  if (lab='quit'); break; endif;
  'q pos'
  x = subwrd(result,3)
  y = subwrd(result,4)
  'set line 0'
  'draw recf '%(x-0.15)%' '%(y-0.07)%' '%(x+0.15)%' '%(y+0.07)
  'draw string 'x' 'y' 'lab
endwhile



