'open ht.ctl'

'd ht'
r=ellipse(20, 30, 1.15, 0.75, 30)


function ellipse(x1, y1, r1, r2, theta) 
 'query w2xy 'x1 ' ' y1 
 xl1 = subwrd(result,3)
 yl1 = subwrd(result,6)
 rtheta=theta*3.14/180
 'd cos('rtheta')'
 rcos = subwrd(result,4)
 'd sin('rtheta')'
 rsin = subwrd(result,4) 

 rdx = r1 * rcos 
 rdy = r1 * rsin

 xx1 = xl1 + rdx
 yy1 = yl1 + rdy
 i = 0
 while ( i <= 360 )
   i = i + 15
   phi = i*3.14/180
  'd cos('phi')'
   dx = r1 * subwrd(result,4)
  'd sin('phi')'
   dy = r2 * subwrd(result,4)
   rdx = rcos * dx - rsin * dy
   rdy = rsin * dx + rcos * dy
   xx2 = xl1 + rdx
   yy2 = yl1 + rdy
   'draw line 'xx1' 'yy1' 'xx2' 'yy2
   xx1 = xx2
   yy1 = yy2
 endwhile
return





