function drawrec(args)
x1 = subwrd(args,1)
y1 = subwrd(args,2)
x2 = subwrd(args,3)
y2 = subwrd(args,4)
'query w2xy 'x1 ' ' y1 
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
'query w2xy 'x2 ' 'y2
xl2 = subwrd(result,3)
yl2 = subwrd(result,6)
'draw recf 'xl1 ' 'yl1 ' 'xl2  ' 'yl2
