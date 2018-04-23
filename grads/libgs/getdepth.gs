function getdepth()

'q dim'
c=sublin(result,4)
depth=subwrd(c,6)
zgrid=subwrd(c,9)
depth=depth%'m (k:'%zgrid%')'

return depth
