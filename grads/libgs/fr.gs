function fr ( args )
file1 = subwrd(args,1)
*
'q files'
line0 = sublin(result,1)
file0 = subwrd(line0,2)
*
'q dims'
xline = sublin(result,2)
yline = sublin(result,3)
zline = sublin(result,4)
tline = sublin(result,5)
*
'set dfile 'file1
*
xterm = subwrd(xline,3)
if (xterm = 'fixed' )
      x1 = subwrd(xline,6)
      'set lon 'x1
endif
if (xterm = 'varying' )
      x1 = subwrd(xline,6)
      x2 = subwrd(xline,8)
      'set lon 'x1' 'x2      
endif
*
yterm = subwrd(yline,3)     
if (yterm = 'fixed' )
      y1 = subwrd(yline,6)
      'set lat 'y1
endif
if (yterm = 'varying' )
      y1 = subwrd(yline,6)
      y2 = subwrd(yline,8)
      'set lat 'y1' 'y2      
endif 
*
zterm = subwrd(zline,3)
if (zterm = 'fixed' )
      z1 = subwrd(zline,6)
      'set lev 'z1
endif
if (zterm = 'varying' )
      z1 = subwrd(zline,6)
      z2 = subwrd(zline,8)
      'set lev 'z1' 'z2      
endif 
* 
t1 = subwrd(tline,9)
'set t 't1
return


