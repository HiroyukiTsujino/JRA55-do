'open HISTORY3'
'set grads off'
'set x 1'
'set y 1'
'set z 1 40'
'set yflip on'
'collect 1 free'
lon.1 = -10.5
lon.2 = -13.5
lon.3 = -16.5
lon.4 = -19.5
lon.5 = -22.5
lat.1 = 51.5
lat.2 = 51.5
lat.3 = 51.5
lat.4 = 51.5
lat.5 = 51.5
i = 1
while (i <= 5)
'collect 1 gr2stn(tem,'lon.i','lat.i')'
i = i + 1
endwhile
'set x 1 10'
'set clab on'
'd coll2gr(1)'





