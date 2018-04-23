function colmap(args)

typ=subwrd(args,1)

*
*    bluered/redblue                                 (20 colors)
*    odcm1/rodcm1                                    (14 colors)
*    odcm2/rodcm2                                    (21 colors)
*    odcm3/rodcm3                  (8/19 Satoshi S.) (31 colors)
*    odcm40/rodcm40                (8/19 Satoshi S.) (40 colors)
*    odcm4/rodcm4                  (8/19 Satoshi S.) (11 colors)
*    odcm7/rodcm7                                    (11 colors)
*    odcm8/rodcm8                                    (11 colors)
*    jmasst/rjmasst                (8/19 Satoshi S.) (31 colors)
*    xrbow/rxrbow   (xcols.gs used,  2/20/98 ishii)  (15 colors)
*    rb26g/rrb26g     (6/3/98 ishii)
*    rb26/rrb26     (6/3/98 ishii)
*    rb40/rrb40     (6/3/98 ishii)
*    topo25/rtopo25
*
*    factor20/rfactor20  for correction factor 
*
*    rb44/rrb44     (Sakamoto)
*    rb64/rrb64     (Sakamoto)
*    rbb64/rrbb64     (Sakamoto) blue - light blue - white - yellow - red
*    rbow60/rrbow60     (Sakamoto)
*    rbow54     (Sakamoto)
*    grbow38/rgrbow38     (Sakamoto)
*    mono44/rmono44     (Sakamoto)
*    mono12/rmono12     (Sakamoto)
*    mono14/rmono14     (Sakamoto)
*    mono6/rmono6     (Sakamoto)
*    mono5/rmono5     (Sakamoto)
*    mono4/rmono4     (Sakamoto)
*    bwb12     (Sakamoto) black - white - black
*    bwb24     (Sakamoto)
*
* Final Ed. 2/20/98  ishii


if(typ='')
  typ=bluered
endif

dc=1/10
if(typ='bluered' | typ='redblue')
  i=1
  while (i<=20)
    if(i<=10)
      red=i*dc*255
      green=(red+255)/2
      blue=255
    else
      red=255
      blue=(20-i)*dc*255
      green=(blue+255)/2
    endif
*    say red' 'green' 'blue
    
    'set rgb 'i+20' 'red' 'green' 'blue
    i=i+1
  endwhile
  if(typ='bluered') 
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40'
  else
    'set rbcols 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='bluered5' | typ='redblue5')
  "set rgb 20 51 153 255"
  "set rgb 21 102 178.5 255"
  "set rgb 22 153 204 255"
  "set rgb 23 204 229.5 255"
  "set rgb 24 255   255     255"
  "set rgb 25 255 229.5 204"
  "set rgb 26 255 204 153"
  "set rgb 27 255 178.5 102"
  "set rgb 28 255 153 51"
  if(typ='bluered5')
    'set rbcols 20 21 22 23 24 25 26 27 28'
  else
    'set rbcols 28 27 26 25 24 23 22 21 20'
  endif
endif

if(typ='odcm0' | typ='rodcm0')
  "set rgb 20   0   0 150"
  "set rgb 21  50  50 255"
  "set rgb 22 100 100 255"
  "set rgb 23 180 180 255"
  "set rgb 24 230 230 255"
  "set rgb 25 255 220 220"
  "set rgb 26 255 180 180"
  "set rgb 27 255 100 100"
  "set rgb 28 255  50  50"
  "set rgb 29 150   0   0"
  if(typ='odcm0')
    'set rbcols 20 21 22 23 24 25 26 27 28 29'
  else
    'set rbcols 29 28 27 26 25 24 23 22 21 20'
  endif
endif

if(typ='odcm1' | typ='rodcm1')
  "set rgb 20    0   0 130"
  "set rgb 21    0   0 255"
  "set rgb 22   60  60 255"
  "set rgb 23  120 120 255"
  "set rgb 24  150 150 255"
  "set rgb 25  200 200 255" 
  "set rgb 26  230 230 255"
  "set rgb 27  255 230 230"
  "set rgb 28  255 200 200"
  "set rgb 29  255 150 150"
  "set rgb 30  255 120 120"
  "set rgb 31  255  60  60"
  "set rgb 32  255   0   0"
  "set rgb 33  255   0 120"
  if(typ='odcm1')
    'set rbcols 20 21 22 23 24 25 26 27 28 29 30 31 32 33'
  else
    'set rbcols 33 32 31 30 29 28 27 26 25 24 23 22 21 20'
  endif
endif

if(typ='odcm2' | typ='rodcm2')
  "set rgb 41   200   0  253"
  "set rgb 42   150   0  230"
  "set rgb 43     0   0    253"
  "set rgb 44    0   124    253"
  "set rgb 45    4   188    253"
  "set rgb 46    4   220    251"
  "set rgb 47    3   251    251"
  "set rgb 48    1   251    165"
  "set rgb 49    53   253    12"
  "set rgb 50    124   253    83"
  "set rgb 51    191   253    110"
  "set rgb 52    214   251    194"
  "set rgb 53    253   253    139"
  "set rgb 54    253   227    0"
  "set rgb 55    251   191    80"
  "set rgb 56    253   160    84"
  "set rgb 57    251   127    60"
  "set rgb 58    253   86    50"
  "set rgb 59    253   0    0"
  "set rgb 60    225   0    0"
  "set rgb 61    200   0    0"
  if(typ='odcm2')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41'
  endif
endif

if(typ='odcm2b' | typ='rodcm2b')
  "set rgb 41     0   0    130"
  "set rgb 42     0   0    253"
  "set rgb 43     0   60   253"
  "set rgb 44    0   124    253"
  "set rgb 45    4   188    253"
  "set rgb 46    4   220    251"
  "set rgb 47    3   251    251"
  "set rgb 48    1   251    165"
  "set rgb 49    53   253    12"
  "set rgb 50    124   253    83"
  "set rgb 51    191   253    110"
  "set rgb 52    214   251    194"
  "set rgb 53    253   253    139"
  "set rgb 54    253   227    0"
  "set rgb 55    251   191    80"
  "set rgb 56    253   160    84"
  "set rgb 57    251   127    60"
  "set rgb 58    253   86    50"
  "set rgb 59    253   0    0"
  "set rgb 60    225   0    0"
  "set rgb 61    200   0    0"
  if(typ='odcm2b')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41'
  endif
endif

if(typ='odcm2n' | typ='rodcm2n')
  "set rgb 41    253   253  253"
  "set rgb 42   150   0  230"
  "set rgb 43     0   0    253"
  "set rgb 44    0   124    253"
  "set rgb 45    4   188    253"
  "set rgb 46    4   220    251"
  "set rgb 47    3   251    251"
  "set rgb 48    1   251    165"
  "set rgb 49    53   253    12"
  "set rgb 50    124   253    83"
  "set rgb 51    191   253    110"
  "set rgb 52    214   251    194"
  "set rgb 53    253   253    139"
  "set rgb 54    253   227    0"
  "set rgb 55    251   191    80"
  "set rgb 56    253   160    84"
  "set rgb 57    251   127    60"
  "set rgb 58    253   86    50"
  "set rgb 59    253   0    0"
  "set rgb 60    225   0    0"
  "set rgb 61    200   0    0"
  if(typ='odcm2n')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41'
  endif
endif

if(typ='odcm3' | typ='rodcm3')
  "set rgb 31    0   0    130"
  "set rgb 32    0   0    253"
  "set rgb 33    0   0    253"
  "set rgb 34    0   50   253"
  "set rgb 35    0   90   253"
  "set rgb 36    0   125  253"
  "set rgb 37    4   160  253"
  "set rgb 38    4   190  253"
  "set rgb 39    4   220  251"
  "set rgb 40    3   251  251"
  "set rgb 41    1   251  165"
  "set rgb 42    10  251  90"
  "set rgb 43    53  253  12"
  "set rgb 44    90  253  50"
  "set rgb 45    124 253  83"
  "set rgb 46    160 253  110"
  "set rgb 47    191 253  150"
  "set rgb 48    214 253  194"
  "set rgb 49    253 253  160"
  "set rgb 50    253 245  120"
  "set rgb 51    253 237  65"
  "set rgb 52    253 227  0"
  "set rgb 53    251 210  50"
  "set rgb 54    251 191  80"
  "set rgb 55    252 160  80"
  "set rgb 56    252 127  65"
  "set rgb 57    252 86   50"
  "set rgb 58    253 40   30"
  "set rgb 59    253   0   0"
  "set rgb 60    225   0   0"
  "set rgb 61    200   0   0"
  if(typ='odcm3')
    'set rbcols 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31'
  endif
endif

if(typ='odcm3n' | typ='rodcm3n')
  "set rgb 31    0   0    130"
  "set rgb 32    0   0    203"
  "set rgb 33    0   0    253"
  "set rgb 34    0   50   253"
  "set rgb 35    0   90   253"
  "set rgb 36    0   125  253"
  "set rgb 37    4   160  253"
  "set rgb 38    4   190  253"
  "set rgb 39    4   220  251"
  "set rgb 40    3   251  251"
  "set rgb 41    1   251  165"
  "set rgb 42    10  251  90"
  "set rgb 43    53  253  12"
  "set rgb 44    90  253  50"
  "set rgb 45    124 253  83"
  "set rgb 46    160 253  110"
  "set rgb 47    191 253  150"
  "set rgb 48    214 253  194"
  "set rgb 49    253 253  160"
  "set rgb 50    253 245  120"
  "set rgb 51    253 237  65"
  "set rgb 52    253 227  0"
  "set rgb 53    251 210  50"
  "set rgb 54    251 191  80"
  "set rgb 55    252 160  80"
  "set rgb 56    252 127  65"
  "set rgb 57    252 86   50"
  "set rgb 58    253 40   30"
  "set rgb 59    253 0    0"
  "set rgb 60    253   0  136"
  "set rgb 61    253 253  253"
  if(typ='odcm3n')
    'set rbcols 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31'
  endif
endif

if(typ='odcm3w' | typ='rodcm3w')
  "set rgb 31    253 253  253"
  "set rgb 32    0   0    203"
  "set rgb 33    0   0    253"
  "set rgb 34    0   50   253"
  "set rgb 35    0   90   253"
  "set rgb 36    0   125  253"
  "set rgb 37    4   160  253"
  "set rgb 38    4   190  253"
  "set rgb 39    4   220  251"
  "set rgb 40    3   251  251"
  "set rgb 41    1   251  165"
  "set rgb 42    10  251  90"
  "set rgb 43    53  253  12"
  "set rgb 44    90  253  50"
  "set rgb 45    124 253  83"
  "set rgb 46    160 253  110"
  "set rgb 47    191 253  150"
  "set rgb 48    214 253  194"
  "set rgb 49    253 253  160"
  "set rgb 50    253 245  120"
  "set rgb 51    253 237  65"
  "set rgb 52    253 227  0"
  "set rgb 53    251 210  50"
  "set rgb 54    251 191  80"
  "set rgb 55    252 160  80"
  "set rgb 56    252 127  65"
  "set rgb 57    252 86   50"
  "set rgb 58    253 40   30"
  "set rgb 59    253 0    0"
  "set rgb 60    253 0  60"
  "set rgb 61    253 0  120"
  if(typ='odcm3w')
    'set rbcols 0 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31'
  endif
endif

if(typ='odcm40' | typ='rodcm40')
  "set rgb 28   130   0   220"
  "set rgb 29   110   0   200"
  "set rgb 30    90   0   180"
  "set rgb 31    50  0    160"
  "set rgb 32    0   0    203"
  "set rgb 33    0   0    253"
  "set rgb 34    0   50   253"
  "set rgb 35    0   90   253"
  "set rgb 36    0   125  253"
  "set rgb 37    4   160  253"
  "set rgb 38    4   190  253"
  "set rgb 39    4   220  251"
  "set rgb 40    3   251  251"
  "set rgb 41    1   251  165"
  "set rgb 42    10  251   90"
  "set rgb 43    53  253   12"
  "set rgb 44    90  253   50"
  "set rgb 45    124 253   83"
  "set rgb 46    160 253  110"
  "set rgb 47    191 253  150"
  "set rgb 48    214 253  194"
  "set rgb 49    253 253  160"
  "set rgb 50    253 245  120"
  "set rgb 51    253 237   65"
  "set rgb 52    253 227    0"
  "set rgb 53    251 210   50"
  "set rgb 54    251 191   80"
  "set rgb 55    252 160   80"
  "set rgb 56    252 127   65"
  "set rgb 57    252  86   50"
  "set rgb 58    253  40   30"
  "set rgb 59    253   0    0"
  "set rgb 60    253  20   20"
  "set rgb 61    253  40   40"
  "set rgb 62    253  60   60"
  "set rgb 63    253  80   80"
  "set rgb 64    253 100  100"
  "set rgb 65    253 120  120"
  "set rgb 66    253 150  150"
  "set rgb 67    253 200  200"
  "set rgb 68    253 240  240"
  if(typ='odcm40')
    'set rbcols 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68'
  else
    'set rbcols 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28'
  endif
endif

if(typ='odcm4' | typ='rodcm4')
*  "set rgb 41     20    20    20"
  "set rgb 41      0     0   180"
  "set rgb 42      0     0   253"
  "set rgb 43      4   188   253"
  "set rgb 44      3   251   251"
  "set rgb 45     53   253    12"
  "set rgb 46    191   253   110"
  "set rgb 47    253   253   139"
  "set rgb 48    251   191    80"
  "set rgb 49    251   127    60"
  "set rgb 50    253     0     0"
  "set rgb 51    200     0     0"
  if(typ='odcm4')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51'
  else
    'set rbcols 51 50 49 48 47 46 45 44 43 42 41'
  endif
endif

if(typ='odcm8' | typ='rodcm8')
  "set rgb 41      0     0   240"
  "set rgb 42      0    60   160"
  "set rgb 43      0   120    80"
  "set rgb 44      0   180     0"
  "set rgb 45     80   240     0"
  "set rgb 46    160   240     0"
  "set rgb 47    240   240     0"
  "set rgb 48    240   180     0"
  "set rgb 49    240   120     0"
  "set rgb 50    240    60     0"
  "set rgb 51    240     0     0"
  if(typ='odcm8')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51'
  else
    'set rbcols 51 50 49 48 47 46 45 44 43 42 41'
  endif
endif

if(typ='odcm7' | typ='rodcm7')
  "set rgb 40      0    60   240"
  "set rgb 41      0   240   240"
  "set rgb 42     30   240   180"
  "set rgb 43     60   240   120"
  "set rgb 44    120   240    60"
  "set rgb 45    180   240    30"
  "set rgb 46    240   240     0"
  "set rgb 47    240   200     0"
  "set rgb 48    240   160     0"
  "set rgb 49    240   120     0"
  "set rgb 50    240    60     0"
  "set rgb 51    240     0     0"
  if(typ='odcm7')
    'set rbcols 40 41 42 43 44 45 46 47 48 49 50 51'
  else
    'set rbcols 51 50 49 48 47 46 45 44 43 42 41 40'
  endif
endif

if(typ='odcm5' | typ='rodcm5')
  "set rgb 41    0   0     65"
  "set rgb 42    0   0    130"
  "set rgb 43    0   0    253"
  "set rgb 44    0   124    253"
  "set rgb 45    4   188    253"
  "set rgb 46    4   220    251"
  "set rgb 47    3   251    251"
  "set rgb 48    1   251    165"
  "set rgb 49    53   253    12"
  "set rgb 50    124   253    83"
  "set rgb 51    191   253    110"
  "set rgb 52    214   251    194"
  "set rgb 53    253   253    139"
  "set rgb 54    253   227    0"
  "set rgb 55    251   191    80"
  "set rgb 56    253   160    84"
  "set rgb 57    251   127    60"
  "set rgb 58    253   86    50"
  "set rgb 59    253   0    0"
  "set rgb 60    225   0   0"
  "set rgb 61    200   0   0"
  if(typ='odcm5')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41'
  endif
endif

if(typ='odcm6' | typ='rodcm6')
  "set rgb 31   200   0  253"
  "set rgb 32   150   0  230"
  "set rgb 33   100   0  215"
  "set rgb 34    0    0  215"
  "set rgb 35    0   50  253"
  "set rgb 36    0   90  253"
  "set rgb 37    0   125 253"
  "set rgb 38    0   160 253"
  "set rgb 39    0   190 253"
  "set rgb 40    0   220 251"
  "set rgb 41    0   253 251"
  "set rgb 42    0   253 165"
  "set rgb 43    12  253  83"
  "set rgb 44    53  253   0"
  "set rgb 45    90  253  50"
  "set rgb 46    160 253 110"
  "set rgb 47    214 253 194"
  "set rgb 48    253 253 160"
  "set rgb 49    253 245 120"
  "set rgb 50    253 237  65"
  "set rgb 51    253 227   0"
  "set rgb 52    253 210  50"
  "set rgb 53    253 191  80"
  "set rgb 54    253 160  80"
  "set rgb 55    253 120  60"
  "set rgb 56    253  80  40"
  "set rgb 57    253  40  20"
  "set rgb 58    225   0   0"
  "set rgb 59    200   0   0"
  "set rgb 60    175   0   0"
  "set rgb 61    150   0   0"
  if(typ='odcm6')
    'set rbcols 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61'
  else
    'set rbcols 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31'
  endif
endif

if(typ='factor20' | typ='rfactor20')
  "set rgb 41   200   0  253"
  "set rgb 42   150   0  230"
  "set rgb 43     0   0  253"
  "set rgb 44     0 124  253"
  "set rgb 45     4 188  253"
  "set rgb 46     4 220  251"
  "set rgb 47     3 251  251"
  "set rgb 48    53 253   12"
  "set rgb 49   160 253   80"
  "set rgb 50   230 253  230"
  "set rgb 51   253 253  230"
  "set rgb 52   253 245  120"
  "set rgb 53   253 227    0"
  "set rgb 54   251 191   80"
  "set rgb 55   253 160   84"
  "set rgb 56   251 127   60"
  "set rgb 57   253  86   50"
  "set rgb 58   253   0    0"
  "set rgb 59   225   0    0"
  "set rgb 60   200   0    0"
  if(typ='factor20')
    'set rbcols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60'
  else
    'set rbcols 60 59 58 57 56 55 54 53 51 52 50 49 48 47 46 45 44 43 42 41'
  endif
endif


if(typ='xrbow' | typ='rxrbow')
  cols="MediumPurple SlateBlue1 SkyBlue1 LightSkyBlue1 CadetBlue1 aquamarine1 SeaGreen1 OliveDrab1 khaki1 goldenrod1 sienna1 coral1 OrangeRed1  VioletRed1 magenta1"
  'xcols ' '"'cols '"'
  if(typ='xrbow')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35'
  else
    'set rbcols 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='rb26g' | typ='rrb26g')
  "set rgb   21   0   0 243"
  "set rgb   22  51  51 243"
  "set rgb   23  77  77 243"
  "set rgb   24 102 102 243"
  "set rgb   25 128 128 243"
  "set rgb   26 141 141 243"
  "set rgb   27 154 154 243"
  "set rgb   28 166 166 243"
  "set rgb   29 179 179 243"
  "set rgb   30 192 192 243"
  "set rgb   31 205 205 243"
  "set rgb   32 218 218 243"
  "set rgb   33 243 255 243"
  "set rgb   34 243 255 243"
  "set rgb   35 243 218 218"
  "set rgb   36 243 205 205"
  "set rgb   37 243 192 192"
  "set rgb   38 243 179 179"
  "set rgb   39 243 166 166"
  "set rgb   40 243 154 154"
  "set rgb   41 243 141 141"
  "set rgb   42 243 128 128"
  "set rgb   43 243 102 102"
  "set rgb   44 243  77  77"
  "set rgb   45 243  51  51"
  "set rgb   46 243   0   0"
  if(typ='rb26g')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46'
  else
    'set rbcols 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='rb26' | typ='rrb26')
  "set rgb   21   0   0 243"
  "set rgb   22  51  51 243"
  "set rgb   23  77  77 243"
  "set rgb   24 102 102 243"
  "set rgb   25 128 128 243"
  "set rgb   26 141 141 243"
  "set rgb   27 154 154 243"
  "set rgb   28 166 166 243"
  "set rgb   29 179 179 243"
  "set rgb   30 192 192 243"
  "set rgb   31 205 205 243"
  "set rgb   32 218 218 243"
  "set rgb   33 230 230 243"
  "set rgb   34 243 230 230"
  "set rgb   35 243 218 218"
  "set rgb   36 243 205 205"
  "set rgb   37 243 192 192"
  "set rgb   38 243 179 179"
  "set rgb   39 243 166 166"
  "set rgb   40 243 154 154"
  "set rgb   41 243 141 141"
  "set rgb   42 243 128 128"
  "set rgb   43 243 102 102"
  "set rgb   44 243  77  77"
  "set rgb   45 243  51  51"
  "set rgb   46 243   0   0"
  if(typ='rb26')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46'
  else
    'set rbcols 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='rb40' | typ='rrb40')
  "set rgb   21   0   0 243"
  "set rgb   22  26  26 243"
  "set rgb   23  51  51 243"
  "set rgb   24  64  64 243"
  "set rgb   25  77  77 243"
  "set rgb   26  90  90 243"
  "set rgb   27 102 102 243"
  "set rgb   28 115 115 243"
  "set rgb   29 128 128 243"
  "set rgb   30 141 141 243"
  "set rgb   31 154 154 243"
  "set rgb   32 166 166 243"
  "set rgb   33 179 179 243"
  "set rgb   34 192 192 243"
  "set rgb   35 205 205 243"
  "set rgb   36 218 218 243"
  "set rgb   37 223 223 243"
  "set rgb   38 228 228 243"
  "set rgb   39 233 233 243"
  "set rgb   40 238 238 243"
  "set rgb   41 243 238 238"
  "set rgb   42 243 233 233"
  "set rgb   43 243 228 228"
  "set rgb   44 243 223 223"
  "set rgb   45 243 218 218"
  "set rgb   46 243 205 205"
  "set rgb   47 243 192 192"
  "set rgb   48 243 179 179"
  "set rgb   49 243 166 166"
  "set rgb   50 243 154 154"
  "set rgb   51 243 141 141"
  "set rgb   52 243 128 128"
  "set rgb   53 243 115 115"
  "set rgb   54 243 102 102"
  "set rgb   55 243  90  90"
  "set rgb   56 243  77  77"
  "set rgb   57 243  64  64"
  "set rgb   58 243  51  51"
  "set rgb   59 243  26  26"
  "set rgb   60 243   0   0"
  if(typ='rb40')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60'
  else
    'set rbcols 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif


if(typ='rb40w' | typ='rrb40w')
  "set rgb   21   0   0 243"
  "set rgb   22  26  26 243"
  "set rgb   23  51  51 243"
  "set rgb   24  64  64 243"
  "set rgb   25  77  77 243"
  "set rgb   26  90  90 243"
  "set rgb   27 102 102 243"
  "set rgb   28 115 115 243"
  "set rgb   29 128 128 243"
  "set rgb   30 141 141 243"
  "set rgb   31 154 154 243"
  "set rgb   32 166 166 243"
  "set rgb   33 179 179 243"
  "set rgb   34 192 192 243"
  "set rgb   35 205 205 243"
  "set rgb   36 218 218 243"
  "set rgb   37 223 223 243"
  "set rgb   38 228 228 243"
  "set rgb   39 233 233 243"
  "set rgb   40 255 255 255"
  "set rgb   41 255 255 255"
  "set rgb   42 243 233 233"
  "set rgb   43 243 228 228"
  "set rgb   44 243 223 223"
  "set rgb   45 243 218 218"
  "set rgb   46 243 205 205"
  "set rgb   47 243 192 192"
  "set rgb   48 243 179 179"
  "set rgb   49 243 166 166"
  "set rgb   50 243 154 154"
  "set rgb   51 243 141 141"
  "set rgb   52 243 128 128"
  "set rgb   53 243 115 115"
  "set rgb   54 243 102 102"
  "set rgb   55 243  90  90"
  "set rgb   56 243  77  77"
  "set rgb   57 243  64  64"
  "set rgb   58 243  51  51"
  "set rgb   59 243  26  26"
  "set rgb   60 243   0   0"
  if(typ='rb40w')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60'
  else
    'set rbcols 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

dc=1/10
if(typ='purore' | typ='orepur')
  "set rgb 21 128   0 128"
  "set rgb 22 137  18 137"
  "set rgb 23 146  36 146"
  "set rgb 24 155  54 155"
  "set rgb 25 164  72 164"
  "set rgb 26 173  90 173"
  "set rgb 27 182 108 182"
  "set rgb 28 191 126 191"
  "set rgb 29 200 144 200"
  "set rgb 30 209 162 209"
  "set rgb 31 218 180 218"
  "set rgb 32 227 198 227"
  "set rgb 33 236 216 236"
  "set rgb 34 255 236 216"
  "set rgb 35 255 227 198"
  "set rgb 36 255 218 180"
  "set rgb 37 255 209 162"
  "set rgb 38 255 200 144"
  "set rgb 39 255 191 126"
  "set rgb 40 255 182 108"
  "set rgb 41 255 173  90"
  "set rgb 42 255 164  72"
  "set rgb 43 255 155  54"
  "set rgb 44 255 146  36"
  "set rgb 45 255 137  18"
  "set rgb 46 255 128   0"
  if(typ='purore') 
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46'
  else
    'set rbcols 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='topo25' | typ='rtopo25')
  "set rgb 21 235 235 255"
  "set rgb 22 217 217 255"
  "set rgb 23 197 197 255"
  "set rgb 24 179 179 255"
  "set rgb 25 159 159 255"
  "set rgb 26 139 139 255"
  "set rgb 27 121 121 255"
  "set rgb 28  83  83 255"
  "set rgb 29  63  63 255"
  "set rgb 30  45  45 255"
  "set rgb 31  25  25 255"
  "set rgb 32   5   5 255"
  "set rgb 33   0   0 242"
  "set rgb 34   0   0 222"
  "set rgb 35   0   0 204"
  "set rgb 36   0   0 184"
  "set rgb 37   0   0 164"
  "set rgb 38   0   0 146"
  "set rgb 39   0   0 126"
  "set rgb 40   0   0 108"
  "set rgb 41   0   0  88"
  "set rgb 42   0   0  70"
  "set rgb 43   0   0  50"
  "set rgb 44   0   0  30"
  "set rgb 45   0   0  12"
  if(typ='topo25') 
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45'
  else
    'set rbcols 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='aquawhite' | typ='raquawhite')
  "set rgb 20 255 255 255"
  "set rgb 21   0  0  255"
  "set rgb 22   0  80 255"
  "set rgb 23   0 160 255"
  "set rgb 24   0 210 255"
  "set rgb 25   0 255 255"
  "set rgb 26  80 255 255"
  "set rgb 27 160 255 255"
  "set rgb 28 210 255 255"
  "set rgb 29 255 255 255"
  if(typ='aquawhite')
    'set rbcols 20 21 22 23 24 25 26 27 28 29'
  else
    'set rbcols 29 28 27 26 25 24 23 22 21 20'
  endif
endif

*---- Sakamoto ----

if(typ='rb44' | typ='rrb44')
  "set rgb   21   0   0 253"
  "set rgb   22  26  26 253"
  "set rgb   23  51  51 253"
  "set rgb   24  64  64 253"
  "set rgb   25  77  77 253"
  "set rgb   26  90  90 253"
  "set rgb   27 102 102 253"
  "set rgb   28 115 115 253"
  "set rgb   29 128 128 253"
  "set rgb   30 141 141 253"
  "set rgb   31 154 154 253"
  "set rgb   32 166 166 253"
  "set rgb   33 179 179 253"
  "set rgb   34 192 192 253"
  "set rgb   35 205 205 253"
  "set rgb   36 218 218 253"
  "set rgb   37 223 223 253"
  "set rgb   38 228 228 253"
  "set rgb   39 233 233 253"
  "set rgb   40 238 238 253"
  "set rgb   41 243 243 253"
  "set rgb   42 248 248 253"
  "set rgb   43 253 248 243"
  "set rgb   44 253 243 243"
  "set rgb   45 253 238 238"
  "set rgb   46 253 233 233"
  "set rgb   47 253 228 228"
  "set rgb   48 253 223 223"
  "set rgb   49 253 218 218"
  "set rgb   50 253 205 205"
  "set rgb   51 253 192 192"
  "set rgb   52 253 179 179"
  "set rgb   53 253 166 166"
  "set rgb   54 253 154 154"
  "set rgb   55 253 141 141"
  "set rgb   56 253 128 128"
  "set rgb   57 253 115 115"
  "set rgb   58 253 102 102"
  "set rgb   59 253  90  90"
  "set rgb   60 253  77  77"
  "set rgb   61 253  64  64"
  "set rgb   62 253  51  51"
  "set rgb   63 253  26  26"
  "set rgb   64 253   0   0"
  if(typ='rb44')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64'
  else
    'set rbcols 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='rbow60' | typ='rrbow60' | typ='grbow38' | typ='rgrbow38' | typ='rbow54')
  "set rgb 21  160    0  253"
  "set rgb 22  140    0  253"
  "set rgb 23  120    0  253"
  "set rgb 24  100    0  253"
  "set rgb 25   80    0  253"
  "set rgb 26   60    0  253"
  "set rgb 27   40    0  253"
  "set rgb 28   20    0  253"
  "set rgb 29    0    0  253"
  "set rgb 30    0   25  253"
  "set rgb 31    0   50  253"
  "set rgb 32    0   70  253"
  "set rgb 33    0   90  253"
  "set rgb 34    0  107  253"
  "set rgb 35    0  125  253"
  "set rgb 36    2  142  253"
  "set rgb 37    4  160  253"
  "set rgb 38    4  175  253"
  "set rgb 39    4  190  253"
  "set rgb 40    4  205  252"
  "set rgb 41    4  220  251"
  "set rgb 42    4  235  253"
  "set rgb 43    0  253  253"
  "set rgb 44    0  251  208"
  "set rgb 45    1  251  165"
  "set rgb 46    5  251  127"
  "set rgb 47    5  251   90"
  "set rgb 48   21  253   51"
  "set rgb 49   43  253    2"
  "set rgb 50   71  253   31"
  "set rgb 51   90  253   50"
  "set rgb 52  107  253   66"
  "set rgb 53  124  253   83"
  "set rgb 54  142  253   96"
  "set rgb 55  160  253  110"
  "set rgb 56  175  253  130"
  "set rgb 57  191  253  150"
  "set rgb 58  202  253  172"
  "set rgb 59  214  253  194"
  "set rgb 60  233  253  177"
  "set rgb 61  253  253  160"
  "set rgb 62  253  245  112"
  "set rgb 63  253  237   65"
  "set rgb 64  253  232   32"
  "set rgb 65  253  227    0"
  "set rgb 66  252  218   25"
  "set rgb 67  251  210   50"
  "set rgb 68  251  185   65"
  "set rgb 69  252  160   80"
  "set rgb 70  252  143   72"
  "set rgb 71  252  127   65"
  "set rgb 72  252  106   57"
  "set rgb 73  252   86   50"
  "set rgb 74  252   63   40"
  "set rgb 75  253   40   30"
  "set rgb 76  253   20   15"
  "set rgb 77  253    0    0"
  "set rgb 78  238    0    0"
  "set rgb 79  225    0    0"
  "set rgb 80  212    0    0"
  "set rgb 81  200    0    0"
  if(typ='rbow60')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81'
  endif
  if(typ='rrbow60')
    'set rbcols 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
  if(typ='grbow38')
    'set rbcols 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81'
  endif
  if(typ='rgrbow38')
    'set rbcols 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43'
  endif
  if(typ='rbow54')
    'set rbcols 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81'
  endif
endif

if(typ='grad50' | typ='rgrad50')
  "set rgb 31 253 253 253"
  "set rgb 32 240 253 253"
  "set rgb 33 230 253 253"
  "set rgb 34 220 253 253"
  "set rgb 35 200 253 253"
  "set rgb 36 175 253 253"
  "set rgb 37 150 253 253"
  "set rgb 38 125 253 253"
  "set rgb 39 100 253 253"
  "set rgb 40  75 253 253"
  "set rgb 41  50 253 253"
  "set rgb 42  25 253 253"
  "set rgb 43    0  253  253"
  "set rgb 44    0  251  208"
  "set rgb 45    1  251  165"
  "set rgb 46    5  251  127"
  "set rgb 47    5  251   90"
  "set rgb 48   21  253   51"
  "set rgb 49   43  253    2"
  "set rgb 50   71  253   31"
  "set rgb 51   90  253   50"
  "set rgb 52  107  253   66"
  "set rgb 53  124  253   83"
  "set rgb 54  142  253   96"
  "set rgb 55  160  253  110"
  "set rgb 56  175  253  130"
  "set rgb 57  191  253  150"
  "set rgb 58  202  253  172"
  "set rgb 59  214  253  194"
  "set rgb 60  233  253  177"
  "set rgb 61  253  253  160"
  "set rgb 62  253  245  112"
  "set rgb 63  253  237   65"
  "set rgb 64  253  232   32"
  "set rgb 65  253  227    0"
  "set rgb 66  252  218   25"
  "set rgb 67  251  210   50"
  "set rgb 68  251  185   65"
  "set rgb 69  252  160   80"
  "set rgb 70  252  143   72"
  "set rgb 71  252  127   65"
  "set rgb 72  252  106   57"
  "set rgb 73  252   86   50"
  "set rgb 74  252   63   40"
  "set rgb 75  253   40   30"
  "set rgb 76  253   20   15"
  "set rgb 77  253    0    0"
  "set rgb 78  238    0    0"
  "set rgb 79  225    0    0"
  "set rgb 80  212    0    0"
  "set rgb 81  204    0    0"
  if(typ='grad50')
    'set rbcols 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81'
  endif
endif

if(typ='gray' | typ='rgray')
  "set rgb 20    0   0   0"
  "set rgb 21  200 200 200"
  "set rgb 22  255 255 255"
  if(typ='gray')
    'set rbcols 20 21'
  else
    'set rbcols 21 22'
  endif
endif

if(typ='rb64' | typ='rrb64' | typ='rb32')
  "set rgb   21   0 253 253"
  "set rgb   22   0 248 253"
  "set rgb   23   0 231 253"
  "set rgb   24   0 215 253"
  "set rgb   25   0 198 253"
  "set rgb   26   0 182 253"
  "set rgb   27   0 165 253"
  "set rgb   28   0 149 253"
  "set rgb   29   0 132 253"
  "set rgb   30   0 116 253"
  "set rgb   31   0  99 253"
  "set rgb   32   0  83 253"
  "set rgb   33   0  66 253"
  "set rgb   34   0  50 253"
  "set rgb   35   0  33 253"
  "set rgb   36   0  17 253"
  "set rgb   37   0   0 253"
  "set rgb   38  17  17 253"
  "set rgb   39  33  33 253"
  "set rgb   40  50  50 253"
  "set rgb   41  66  66 253"
  "set rgb   42  83  83 253"
  "set rgb   43  99  99 253"
  "set rgb   44 116 116 253"
  "set rgb   45 132 132 253"
  "set rgb   46 149 149 253"
  "set rgb   47 165 165 253"
  "set rgb   48 182 182 253"
  "set rgb   49 198 198 253"
  "set rgb   50 215 215 253"
  "set rgb   51 231 231 253"
  "set rgb   52 248 248 253"
  "set rgb   53 253 248 243"
  "set rgb   54 253 231 231"
  "set rgb   55 253 215 215"
  "set rgb   56 253 198 198"
  "set rgb   57 253 182 182"
  "set rgb   58 253 165 165"
  "set rgb   59 253 149 149"
  "set rgb   60 253 132 132"
  "set rgb   61 253 116 116"
  "set rgb   62 253  99  99"
  "set rgb   63 253  83  83"
  "set rgb   64 253  66  66"
  "set rgb   65 253  50  50"
  "set rgb   66 253  33  33"
  "set rgb   67 253  17  17"
  "set rgb   68 253   0   0"
  "set rgb   69 253  17   0"
  "set rgb   70 253  33   0"
  "set rgb   71 253  50   0"
  "set rgb   72 253  66   0"
  "set rgb   73 253  83   0"
  "set rgb   74 253  99   0"
  "set rgb   75 253 116   0"
  "set rgb   76 253 132   0"
  "set rgb   77 253 149   0"
  "set rgb   78 253 165   0"
  "set rgb   79 253 182   0"
  "set rgb   80 253 198   0"
  "set rgb   81 253 215   0"
  "set rgb   82 253 231   0"
  "set rgb   83 253 248   0"
  "set rgb   84 253 253   0"
  if(typ='rb64')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84'
  endif
  if (typ='rrb64')
    'set rbcols 84 83 82 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
  if (typ='rb32')
    'set rbcols 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84'
  endif
endif


if(typ='mono44' | typ='rmono44')
  "set rgb   21   0   0   0"
  "set rgb   22   6   6   6"
  "set rgb   23  12  12  12"
  "set rgb   24  18  18  18"
  "set rgb   25  24  24  24"
  "set rgb   26  30  30  30"
  "set rgb   27  36  36  36"
  "set rgb   28  42  42  42"
  "set rgb   29  48  48  48"
  "set rgb   30  54  54  54"
  "set rgb   31  60  60  60"
  "set rgb   32  66  66  66"
  "set rgb   33  72  72  72"
  "set rgb   34  78  78  78"
  "set rgb   35  84  84  84"
  "set rgb   36  90  90  90"
  "set rgb   37  96  96  96"
  "set rgb   38 102 102 102"
  "set rgb   39 108 108 108"
  "set rgb   40 114 114 114"
  "set rgb   41 120 120 120"
  "set rgb   42 126 126 126"
  "set rgb   43 132 132 132"
  "set rgb   44 138 138 138"
  "set rgb   45 144 144 144"
  "set rgb   46 150 150 150"
  "set rgb   47 156 156 156"
  "set rgb   48 162 162 162"
  "set rgb   49 168 168 168"
  "set rgb   50 174 174 174"
  "set rgb   51 180 180 180"
  "set rgb   52 186 186 186"
  "set rgb   53 192 192 192"
  "set rgb   54 198 198 198"
  "set rgb   55 204 204 204"
  "set rgb   56 210 210 210"
  "set rgb   57 216 216 216"
  "set rgb   58 222 222 222"
  "set rgb   59 228 228 228"
  "set rgb   60 234 234 234"
  "set rgb   61 240 240 240"
  "set rgb   62 246 246 246"
  "set rgb   63 252 252 252"
  "set rgb   64 254 254 254"
  if(typ='mono44')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64'
  else
    'set rbcols 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif


if(typ='mono4' | typ='rmono4')
  "set rgb   22  51  51  51"
  "set rgb   23 119 119 119"
  "set rgb   24 187 187 187"
  "set rgb   25 255 255 255"
  if(typ='mono5')
    'set rbcols 22 23 24 25'
  else
    'set rbcols 25 24 23 22'
  endif
endif

if(typ='mono5' | typ='rmono5')
  "set rgb   22  51  51  51"
  "set rgb   23 102 102 102"
  "set rgb   24 163 163 163"
  "set rgb   25 224 224 224"
  "set rgb   26 255 255 255"
  if(typ='mono5')
    'set rbcols 22 23 24 25 26'
  else
    'set rbcols 26 25 24 23 22'
  endif
endif

if(typ='mono6' | typ='rmono6' | typ='bwb12' )
  "set rgb   21   0   0   0"
  "set rgb   22  51  51  51"
  "set rgb   23 102 102 102"
  "set rgb   24 163 163 163"
  "set rgb   25 224 224 224"
  "set rgb   26 255 255 255"
  if(typ='mono6')
    'set rbcols 21 22 23 24 25 26'
  endif
  if(typ='rmono6')
    'set rbcols 26 25 24 23 22 21'
  endif
  if(typ='bwb12')
    'set rbcols 21 22 23 24 25 26 26 25 24 23 22 21'
  endif
endif


if(typ='mono12' | typ='rmono12' | typ='bwb24')
  "set rgb   21   0   0   0"
  "set rgb   22  23  23  23"
  "set rgb   23  46  46  46"
  "set rgb   24  69  69  69"
  "set rgb   25  92  92  92"
  "set rgb   26 115 115 115"
  "set rgb   27 138 138 138"
  "set rgb   28 161 161 161"
  "set rgb   29 184 184 184"
  "set rgb   30 207 207 207"
  "set rgb   31 230 230 230"
  "set rgb   32 255 255 255"
  if(typ='mono12')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32'
  endif
  if(typ='rmono12')  
    'set rbcols 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
  if(typ='bwb24')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='mono14' | typ='rmono14')
  "set rgb   21   0   0   0"
  "set rgb   22  23  23  23"
  "set rgb   23  46  46  46"
  "set rgb   24  69  69  69"
  "set rgb   25  92  92  92"
  "set rgb   26 115 115 115"
  "set rgb   27 138 138 138"
  "set rgb   28 161 161 161"
  "set rgb   29 184 184 184"
  "set rgb   30 207 207 207"
  "set rgb   31 220 220 220"
  "set rgb   32 232 232 232"
  "set rgb   33 244 244 244"
  "set rgb   34 255 255 255"
  if(typ='mono14')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34'
  else
    'set rbcols 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

if(typ='rbb64' | typ='rrbb64')
  "set rgb   21   0   0 253"
  "set rgb   22   0  17 253"
  "set rgb   23   0  33 253"
  "set rgb   24   0  50 253"
  "set rgb   25   0  66 253"
  "set rgb   26   0  83 253"
  "set rgb   27   0  99 253"
  "set rgb   28   0 116 253"
  "set rgb   29   0 132 253"
  "set rgb   30   0 149 253"
  "set rgb   31   0 165 253"
  "set rgb   32   0 182 253"
  "set rgb   33   0 198 253"
  "set rgb   34   0 215 253"
  "set rgb   35   0 231 253"
  "set rgb   36   0 248 253"
  "set rgb   37   0 253 253"
  "set rgb   38  17 253 253"
  "set rgb   39  33 253 253"
  "set rgb   40  50 253 253"
  "set rgb   41  66 253 253"
  "set rgb   42  83 253 253"
  "set rgb   43  99 253 253"
  "set rgb   44 116 253 253"
  "set rgb   45 132 253 253"
  "set rgb   46 149 253 253"
  "set rgb   47 165 253 253"
  "set rgb   48 182 253 253"
  "set rgb   49 198 253 253"
  "set rgb   50 215 253 253"
  "set rgb   51 231 253 253"
  "set rgb   52 248 253 253"
  "set rgb   53 253 253 243"
  "set rgb   54 253 253 231"
  "set rgb   55 253 253 215"
  "set rgb   56 253 253 198"
  "set rgb   57 253 253 182"
  "set rgb   58 253 253 165"
  "set rgb   59 253 253 149"
  "set rgb   60 253 253 132"
  "set rgb   61 253 253 116"
  "set rgb   62 253 253  99"
  "set rgb   63 253 253  83"
  "set rgb   64 253 253  66"
  "set rgb   65 253 253  50"
  "set rgb   66 253 253  33"
  "set rgb   67 253 253  17"
  "set rgb   68 253 253   0"
  "set rgb   69 253 253   0"
  "set rgb   70 253 248   0"
  "set rgb   71 253 231   0"
  "set rgb   72 253 215   0"
  "set rgb   73 253 198   0"
  "set rgb   74 253 182   0"
  "set rgb   75 253 165   0"
  "set rgb   76 253 149   0"
  "set rgb   77 253 132   0"
  "set rgb   78 253 116   0"
  "set rgb   79 253  99   0"
  "set rgb   80 253  83   0"
  "set rgb   81 253  66   0"
  "set rgb   82 253  50   0"
  "set rgb   83 253  33   0"
  "set rgb   84 253  17   0"
  if(typ='rbb64')
    'set rbcols 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84'
  else
    'set rbcols 84 83 82 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21'
  endif
endif

return
