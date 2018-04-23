function dstring3(args)
x1 = subwrd(args,1)
y1 = subwrd(args,2)
tt1 = subwrd(args,3)
tt2 = subwrd(args,4)
tt3 = subwrd(args,5)
'query w2xy 'x1 ' ' y1
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
*'draw string 'xl1' 'yl1' 'tt1' 'tt2' (`3,`0' tt3 ')'
'draw string 'xl1' 'yl1' 'tt1' 'tt2'`3,`0' tt3
return



      
