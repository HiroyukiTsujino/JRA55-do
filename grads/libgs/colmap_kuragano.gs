function colmap(args)

typ=subwrd(args,1)

*
*    bw/wb                                           (20 colors)
*    rnbw/rrnbw                                      (21 colors)
*    anom/ranom                                      (16 colors)
*
*  edited by  kuragano  Jan/6/2015 


if(typ='')
  typ=rnbw
endif

dc=1/20
if(typ='bw' | typ='wb')
  i=1
  while (i<=20)
      red=i*dc*255
      green=red
      blue=red
*    endif
*   say red' 'green' 'blue
    
    'set rgb 'i+20' 'red' 'green' 'blue
    i=i+1
  endwhile
  if(typ='bw') 
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40'
  else
    'set rbcols 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif


if(typ='rnbw' | typ='rrnbw')
  "set rgb   21  70   0 255"
  "set rgb   22   0   0 255"
  "set rgb   23   0  76 255"
  "set rgb   24   0 130 255"
  "set rgb   25   0 184 255"
  "set rgb   26   0 215 255"  
  "set rgb   27   0 230 255"
  "set rgb   28   0 250 250"
  "set rgb   29 102 250 102"
  "set rgb   30 143 250   0"
  "set rgb   31 194 250   0"
  "set rgb   32 220 250   0"
  "set rgb   33 250 250   0"
  "set rgb   34 255 238   0"  
  "set rgb   35 255 225   0"
  "set rgb   36 255 204   0" 
  "set rgb   37 255 165   0"
  "set rgb   38 255 130   0"
  "set rgb   39 255  95   0"
  "set rgb   40 255   0   0"
  "set rgb   41 255   0 255"
  if(typ='rnbw')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41'
  else
    'set rbcols 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='anom' | typ='ranom')
  "set rgb   21   0   0 255"
  "set rgb   22   0 102 255"
  "set rgb   23   0 153 255"
  "set rgb   24  51 204 255"
  "set rgb   25  76 230 255"  
  "set rgb   26 153 255 255"
  "set rgb   27 204 255 255"
  "set rgb   28 230 255 255"
  "set rgb   29 255 242 255"   
  "set rgb   30 255 230 255"
  "set rgb   31 255 204 255"
  "set rgb   32 255 179 255"
  "set rgb   33 255 153 255"
  "set rgb   34 255 127 204"  
  "set rgb   35 255 102 153"  
  "set rgb   36 255  51 102"
  if(typ='anom')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36'
  else
    'set rbcols 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif



return
