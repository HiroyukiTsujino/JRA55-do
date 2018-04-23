function arshade(args)
*
x1 = subwrd(args,1)
y1 = subwrd(args,2)
angle = subwrd(args,3)
length = subwrd(args,4)
width  = subwrd(args,5)
sizehead = subwrd(args,6)
colorar = subwrd(args,7)
*
pi = 3.141592653
*
'query w2xy 'x1 ' ' y1 
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
cosan = math_cos(angle * pi/180.0)
sinan = math_sin(angle * pi/180.0)
xl2 = xl1 + length * cosan
yl2 = yl1 + length * sinan
xl11 = xl1 + 0.5 * width * sinan
yl11 = yl1 - 0.5 * width * cosan
xl12 = xl1 - 0.5 * width * sinan
yl12 = yl1 + 0.5 * width * cosan

xl21 = xl2 + 0.5 * width * sinan
yl21 = yl2 - 0.5 * width * cosan
xl22 = xl2 - 0.5 * width * sinan
yl22 = yl2 + 0.5 * width * cosan
'set line 'colorar' 1 1'
'draw polyf 'xl11' 'yl11' 'xl12' 'yl12' 'xl22' 'yl22' 'xl21' 'yl21
'draw line 'xl11' 'yl11' 'xl12' 'yl12
'draw line 'xl21' 'yl21' 'xl22' 'yl22

*'draw line 'xl1' 'yl1' 'xl3' 'yl3
'draw line 'xl1' 'yl1' 'xl2' 'yl2
xar1 = xl2 + sizehead * cosan
yar1 = yl2 + sizehead * sinan
xar2 = xl2 + sizehead * sinan
yar2 = yl2 - sizehead * cosan
xar3 = xl2 - sizehead * sinan
yar3 = yl2 + sizehead * cosan
*'draw line 'xar1' 'yar1' 'xar2' 'yar2
*'draw line 'xar1' 'yar1' 'xar3' 'yar3
'draw polyf 'xar1' 'yar1' 'xar2' 'yar2' 'xar3' 'yar3' 'xar1' 'yar1
