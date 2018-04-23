function drawstring(args)
*
xc = subwrd(args,1)
yc = subwrd(args,2)
str = subwrd(args,3)
*
'query w2xy 'xc ' ' yc
xp = subwrd(result,3)
yp = subwrd(result,6)
'draw string 'xp ' 'yp' 'str
