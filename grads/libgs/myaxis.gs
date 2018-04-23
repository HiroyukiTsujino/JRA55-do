function myaxis(args)
*
x1 = subwrd(args,1)
y1 = subwrd(args,2)
angle = subwrd(args,3)
length = subwrd(args,4)
width  = subwrd(args,5)
colorar = subwrd(args,6)
up   = subwrd(args,7)
down = subwrd(args,8)
strsiz0 = subwrd(args,9)
factor  = subwrd(args,10)
nmod=subwrd(args,11)
*

pi = 3.141592653
*
'query w2xy 'x1 ' ' y1 
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
xl1_org = xl1
yl1_org = yl1

cosan = math_cos(angle * pi/180.0)
sinan = math_sin(angle * pi/180.0)

'set line 'colorar' 1 1'

'set strsiz 'strsiz0
if (angle=0)
'set string 1 c'
endif
if (angle=90)
'set string 1 r'
endif

ii=1
while (ii <= up) 
xl2 = xl1 + length * cosan
yl2 = yl1 + length * sinan
xl10 = xl1 - 1 * width * sinan
yl10 = yl1 + 1 * width * cosan
xl11 = xl1 + 0.5 * width * sinan
yl11 = yl1 - 0.5 * width * cosan
xl12 = xl1 - 0.5 * width * sinan
yl12 = yl1 + 0.5 * width * cosan

xl20 = xl2 - 1 * width * sinan
yl20 = yl2 + 1 * width * cosan
xl21 = xl2 + 0.5 * width * sinan
yl21 = yl2 - 0.5 * width * cosan
xl22 = xl2 - 0.5 * width * sinan
yl22 = yl2 + 0.5 * width * cosan

if (ii=1)
 if (angle=0)
  'draw string 'xl10' 'yl10' 0'
 endif
endif

'draw line 'xl1' 'yl1' 'xl2' 'yl2
'draw line 'xl11' 'yl11' 'xl12' 'yl12
'draw line 'xl21' 'yl21' 'xl22' 'yl22
ii2=ii*factor
 if (math_mod(ii,nmod)=0) 
'draw string 'xl20' 'yl20' 'ii2
 endif
xl1=xl2
yl1=yl2
ii=ii+1
endwhile


ii=1
xl1 = xl1_org
yl1 = yl1_org
while ( ii <= down) 
xl2 = xl1 - length * cosan
yl2 = yl1 - length * sinan
xl11 = xl1 + 0.5 * width * sinan
yl11 = yl1 - 0.5 * width * cosan
xl12 = xl1 - 0.5 * width * sinan
yl12 = yl1 + 0.5 * width * cosan

xl20 = xl2 - 1 * width * sinan
yl20 = yl2 + 1 * width * cosan
xl21 = xl2 + 0.5 * width * sinan
yl21 = yl2 - 0.5 * width * cosan
xl22 = xl2 - 0.5 * width * sinan
yl22 = yl2 + 0.5 * width * cosan

ii2=-ii*factor
'draw line 'xl1' 'yl1' 'xl2' 'yl2
'draw line 'xl11' 'yl11' 'xl12' 'yl12
'draw line 'xl21' 'yl21' 'xl22' 'yl22
 if (math_mod(ii,nmod)=0) 
'draw string 'xl20' 'yl20' 'ii2
 endif
xl1=xl2
yl1=yl2
ii=ii+1
endwhile


'set strsiz '0.2
'set string 1 l'
