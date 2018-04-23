function colmap(args)

typ=subwrd(args,1)

if(typ='grey7' | typ='rgrey7')
  "set rgb 20   0   0   0"
  "set rgb 21  50  50  50"
  "set rgb 22  80  80  80"
  "set rgb 23 110 110 110"
  "set rgb 24 140 140 140"
  "set rgb 25 170 170 170"
  "set rgb 26 200 200 200"
  if(typ='odcm0')
    'set rbcols 20 21 22 23 24 25 26'
  else
    'set rbcols 26 25 24 23 22 21 20'
  endif
endif

if(typ='grey5' | typ='rgrey5')
  "set rgb 20 100 100 100"
  "set rgb 21 140 140 140"
  "set rgb 22 180 180 180"
  "set rgb 23 220 220 220"
  "set rgb 24 255 255 255"
  if(typ='grey5')
    'set rbcols 20 21 22 23 24'
  else
    'set rbcols 24 23 22 21 20'
  endif
endif

if(typ='grey3' | typ='rgrey3')
  "set rgb 21 160 160 160"
  "set rgb 22 255 255 255"
  "set rgb 23 200 200 200"
  if(typ='grey3')
    'set rbcols 21 22 23'
  else
    'set rbcols 23 22 21'
  endif
endif

return
