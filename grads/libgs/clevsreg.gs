* Set contour levels at regular intervals
*   based on minimum, maximum and interval values.

function clevsreg(args)

cmin=subwrd(args,1)
cmax=subwrd(args,2)
cint=subwrd(args,3)

im=(cmax-cmin)/cint
say im
i=0
clevs=''
while(i<=im) 
  a.i=cint*i + cmin
  say a.i
  clevs=clevs%' '%a.i
  i=i+1
endwhile

'set clevs 'clevs

return
