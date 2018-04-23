function dstring2(args)
x1 = subwrd(args,1)
y1 = subwrd(args,2)
font=subwrd(args,3)
color=subwrd(args,4)
tt1 = subwrd(args,5)
tt2 = subwrd(args,6)
'query w2xy 'x1 ' ' y1
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
'set string 'color
if (font=0) 
'draw string 'xl1' 'yl1' 'tt1 '`3,`0' tt2
endif
if (font=1) 
'draw string 'xl1' 'yl1' 'tt1 '`3,`1' tt2
endif
if (font=2) 
'draw string 'xl1' 'yl1' 'tt1 '`3,`2' tt2
endif
if (font=3) 
'draw string 'xl1' 'yl1' 'tt1 '`3,`3' tt2
endif
if (font=4) 
'draw string 'xl1' 'yl1' 'tt1 '`3,`4' tt2
endif
return



      
