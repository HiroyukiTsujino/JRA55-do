function taux(args)
'open dfile.ctl'
  lon1 = 120      
  lon2 = 250
  t1 = subwrd(args,1)
  'set t 't1
  latmax = 60
  lat1 = -31
  lat2 = -29
  while (lat1<latmax) 
     'd ave(ave(taux,lon='lon1',lon='lon2'),lat='lat1',lat='lat2')'
     line = sublin(result,4)
     word = subwrd(line,4)
     say word
     lat1 = lat1 + 2
     lat2 = lat2 + 2
  endwhile


