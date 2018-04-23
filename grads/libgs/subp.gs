function subp(args)
l_or_p = subwrd(args,1)
row    = subwrd(args,2)
col    = subwrd(args,3)
n      = subwrd(args,4)

if (l_or_p != "p") 
   if (l_or_p != "l") 
      say 'usage: run subp l_or_p row col n'
      say 
      say '       "l_or_p" specifies whether you are using'
      say '                landscape mode of portrait mode. '
      say '       "row"    is the number of the rows.'
      say '       "col"    is the number of the columns.'
      say '       "n"      specifies on which subpage you would like to draw.'
      return
   endif
endif

if (l_or_p = "l") 
   subpla(row,col,n)
endif
if (l_or_p = "p")
   subppo(row,col,n)
endif

function subpla(row,col,n)
*
* Divide plotting area in rox x col  and define virtual page at the
* intersection of row,col  (Landscape version)
xpage=11.
ypage=8.5
dx=xpage/col
dy=ypage/row
*
colj=mod(n,col)
if ( colj = 0) ;colj=col;endif
rowj= 1 + (n-colj)/col
say n' 'rowj' 'colj' '
x=(colj-1)*dx
y=(rowj-1)*dy
xm=x+dx
ym=y+dy
'set vpage 'x' 'xm' 'y' 'ym
say 'Set plot to 'row','col' at Positions 'x' 'xm' 'y' 'ym
return
function subppo(row,col,n)
*
* Divide plotting area in rox x col  and define virtual page at the
* intersection of row,col  (Portrait version)
xpage=8.5
ypage=11
dx=xpage/col
dy=ypage/row
*
colj=mod(n,col)
if ( colj = 0) ;colj=col;endif
rowj= 1 + (n-colj)/col
say n' 'rowj' 'colj' '
x=(colj-1)*dx
y=(rowj-1)*dy
xm=x+dx
ym=y+dy
'set vpage 'x' 'xm' 'y' 'ym
say 'Set plot to 'row','col' at Positions 'x' 'xm' 'y' 'ym
return
function mod(n,m)
* Computes module of n m
*
i=n/m
len=1
if ( i > 10 ); len = 2; endif;
if (i > 100); len=3;  endif;
k=substr(i,1,len);
return n-k*m
function nextpage()
'print'
'q pos'
'c'
function lastpage()
'print'
'c'
