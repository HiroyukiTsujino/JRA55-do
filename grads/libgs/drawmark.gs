function drawmark(args)
*
mk  = subwrd(args,1)
xc  = subwrd(args,2)
yc  = subwrd(args,3)
siz = subwrd(args,4)
col1 = subwrd(args,5)
col2 = subwrd(args,6)
*
'query w2xy 'xc ' ' yc
xp = subwrd(result,3)
yp = subwrd(result,6)
'set line 'col1
'draw mark 'mk ' 'xp ' 'yp' 'siz
'set line 'col2
