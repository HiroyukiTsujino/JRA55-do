function dmark(args)
marktype = subwrd(args,1)
x1 = subwrd(args,2)
y1 = subwrd(args,3)
size     = subwrd(args,4)
'query w2xy 'x1 ' ' y1
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
'draw mark 'marktype' 'xl1' 'yl1' 'size
return



      
