function drawline(args)
*
* 
*
x1 = subwrd(args,1)
y1 = subwrd(args,2)
x2 = subwrd(args,3)
y2 = subwrd(args,4)
'query w2xy 'x1 ' ' y1 
xl1 = subwrd(result,3)
yl1 = subwrd(result,6)
'query w2xy 'x2 ' 'y2
xl2 = subwrd(result,3)
yl2 = subwrd(result,6)
*
* xmin, xmax, ymin, and ymax can be omitted.
*
xmin = subwrd(args,5)
xmax = subwrd(args,6)
ymin = subwrd(args,7)
ymax = subwrd(args,8)
if ( xmin = '') 
      'draw line 'xl1 ' 'yl1 ' 'xl2  ' 'yl2
      return
endif
'query w2xy 'xmin' 'ymin
xlmin = subwrd(result,3)
ylmin = subwrd(result,6)
'query w2xy 'xmax' 'ymax
xlmax = subwrd(result,3)
ylmax = subwrd(result,6)
*
*
if ( xlmin<= xl1 & xl2 <= xlmax & xl1 <= xlmax & xmin <= xl2 & ylmin <= yl1 & yl1 <= ylmax & yl2 <= ylmax & ylmin <= yl2)
  'draw line 'xl1 ' 'yl1 ' 'xl2  ' 'yl2
endif
if ( xlmin <= xl1 & xl2 <= xlmax & xlmin <= xl2 & xl1 <= xlmax )
  xx1 = xl1
  xx2 = xl2
  if ( yl1 <= ylmin & ylmin <= yl2 & yl2 <= ylmax )
    yy1 = ylmin
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl2 & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmax
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl2 <= ylmin & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmin
     'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl1 & ylmin <= yl2  & yl2 <= ylmax )
    yy1 = ylmax
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
endif
if ( xl1 <= xlmin & xlmin <= xl2 & xl2 <= xlmax )
  xx1 = xlmin
  xx2 = xl2
  if ( ylmin <= yl1 & yl2 <= ylmax )
    yy1 = yl1
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl1 <= ylmin & ylmin <= yl2 & yl2 <= ymax )
    yy1 = ylmin
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl2 & ylmin <= yl1 & yl1 <= ymax )
    yy1 = yl1
    yy2 = ylmax
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl2 <= ylmin & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmin
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl1 & ylmin <= yl2  & yl2 <= ylmax )
    yy1 = ylmax
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
endif
if ( xlmax <= xl2 & xlmin <= xl1 & xl1 <= xlmax )
  xx1 = xl1
  xx2 = xlmax
  if ( ylmin <= yl1 | yl2 <= ylmax )
    yy1 = yl1
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl1 <= ylmin & ylmin <= yl2 & yl2 <= ymax )
    yy1 = ylmin
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl2 & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmax
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl2 <= ylmin & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmin
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl1 & ylmin <= yl2  & yl2 <= ylmax )
    yy1 = ylmax
    yy2 = yl2
  'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
endif
if ( xl2 <= xlmin & xlmin <= xl1 & xl1 <= xlmax )
  xx1 = xl1
  xx2 = xlmin
  if ( ylmin <= yl1 | yl2 <= ylmax )
    yy1 = yl1
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl1 <= ylmin & ylmin <= yl2 & yl2 <= ymax )
    yy1 = ylmin
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl2 & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmax
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl2 <= ylmin & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmin
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl1 & ylmin <= yl2  & yl2 <= ylmax )
    yy1 = ylmax
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
endif      
if ( xlmax <= xl1 & xlmin <= xl2  & xl2 <= xlmax )
  xx1 = xlmax
  xx2 = xl2
  if ( ylmin <= yl1 | yl2 <= ylmax )
    yy1 = yl1
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl1 <= ylmin & ylmin <= yl2 & yl2 <= ymax )
    yy1 = ylmin
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl2 & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmax
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( yl2 <= ylmin & ylmin <= yl1 & yl1 <= ylmax )
    yy1 = yl1
    yy2 = ylmin
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
  if ( ylmax <= yl1 & ylmin <= yl2  & yl2 <= ylmax )
    yy1 = ylmax
    yy2 = yl2
    'draw line 'xx1 ' 'yy1' 'xx2 ' 'yy2
  endif
endif      
return

      
