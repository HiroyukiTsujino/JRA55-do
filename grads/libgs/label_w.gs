*--------------------------------------------
function label_w(args)

x=subwrd(args,1)
y=subwrd(args,2)
len=subwrd(args,3)
angle=subwrd(args,4)
size=subwrd(args,5)
thick=subwrd(args,6)
justify=subwrd(args,7)
lab1=subwrd(args,8)
lab2=subwrd(args,9)
lab3=subwrd(args,10)
lab4=subwrd(args,11)
lab5=subwrd(args,12)
lab6=subwrd(args,13)
lab7=subwrd(args,14)
lab8=subwrd(args,15)

*

if(size='' | size='size');size=0.10;endif;
if(justify='' | justify='justify');justify='c';endif;
size2=size*1.0
'set strsiz ' size ' ' size2 
'set string 1 ' justify ' 'thick ' ' angle
w = size*len/2
h = (size2*1.4)/2 
'query w2xy ' x ' ' y
 x = subwrd(result,3)
 y = subwrd(result,6)
'set line 0'
if(angle=0)
 'draw recf '%(x-w)%' '%(y-h*1.2)%' '%(x+w)%' '%(y+h)
endif
'draw string ' x ' ' y ' ' lab1' 'lab2' 'lab3' 'lab4' 'lab5' 'lab6' 'lab7' 'lab8
*
* reset
*
'set line 1'
'set string 1 c 1 0'
*--------------------------------------------
