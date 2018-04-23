function zct(args)
ocean    = subwrd(args,1)
location = subwrd(args,2)
if (location = 'upper')
      'set parea 1.0 10.5 3.0 8.0'
endif
if (location = 'lower')
      'set parea 1.0 10.5 0.5 5.5'
endif
'open HISTORY3'
'set grads off'
'set font 1'
'set x 1'
'set lat -90 90'
'set lev 0 5900'
'set yflip on'
'set gxout contour'
'set cint 0.1'
'set cthick 2'
'set rbrange 20 30'
'set clopts 0 -1 0.12'
'set clskip 3'
'set ylint 1000'
'set xlopts 1 5 0.24'
'set ylopts 1 5 0.24'
if (ocean = 'global')
      'd ave(pd,x=1,x=120)'
endif
if (ocean = 'Atlantic')
      'd ave(Atpd,x=1,x=120)'
endif
if (ocean = 'Pacific')
      'd ave(Pcpd,x=1,x=120)'
endif
if (ocean = 'Indian')
      'd ave(Inpd,x=1,x=120)'
endif
**** geometry *****
'open geo'
'set dfile 2'
'set gxout grfill'
'set lat -90 90'
'set lev 0 5900'
'set x   1'
'set t   1'
'set yflip on'
if (ocean = 'global')
      'run geopaint geo'
endif
if (ocean = 'Atlantic')
      'run geopaint Atgeo'
endif
if (ocean = 'Pacific')
      'run geopaint Pcgeo'
endif
if (ocean = 'Indian')
      'run geopaint Ingeo'
endif
****** option *******
if (location = 'upper')
      'set line 0'
      'draw recf 1.0 3.0 1.6 3.8'
      'draw recf 9.0 3.0 10.5 3.4'
      'set line 1'
      'draw rec 1.05 3.06 1.55 3.75'
      'set string 1 bc 8 0'
      'set strsiz 0.4'
      tt = subwrd(args,3)
      'draw string 1.3 3.2 'tt
      'set strsiz 0.22'
      'draw string 9.75 3.1 'ocean
      'set string 1 bc 4 0'
      'set strsiz 0.36'
endif
if (location = 'lower')
      'set line 0'
      'draw recf 1.0 0.5 1.6 1.3'
      'draw recf 9.0 0.5 10.5 0.9'
      'set line 1'
      'draw rec 1.05 0.56 1.55 1.25'
      'set string 1 bc 8 0'
      'set strsiz 0.4'
      tt = subwrd(args,3)
      'draw string 1.3 0.7 'tt
      'set strsiz 0.22'
      'draw string 9.75 0.6 'ocean
      'set string 1 bc 4 0'
      'set strsiz 0.36'
endif





