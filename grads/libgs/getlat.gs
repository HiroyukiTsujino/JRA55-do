* Return latitude.
function getlat()

'q dims'
c=sublin(result,3)
lat=subwrd(c,6)

if (lat<0)
  lat=-lat
  lat=lat'S'
else
  lat=lat'N'
endif

return lat
