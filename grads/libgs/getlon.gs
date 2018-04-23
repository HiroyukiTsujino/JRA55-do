* Return longitutde.
function getlon()

'q dims'
c=sublin(result,2)
lon=subwrd(c,6)

if (lon>180)
  lon=lon-360
endif
if (lon<0)
  lon=-lon'W'
else
  lon=lon'E'
endif

return lon

