function rb (args)
min = subwrd(args,1)
max = subwrd(args,2)
if ( min > max ) 
  say 'error'
  return
endif
int = intset( min, max )
'set cmin 'min
'set cmax 'max
'set rbrange 'min' 'max
'set cint 'int
return

function intset ( min, max )
*
range0 = 4
range = max - min
if ( range >= range0 )
*** bigger and bigger
   int1 = 0.2
   min1 = 4
   max1 = 8
   min2 = min1
   max2 = max1
   flag = 1
   n    = 1
   m    = 1
   while (flag)
      if ( range >= min2 & range <= max2 ) 
         int = int1
         flag = 0
      endif
* cint is 1 , 2, or 5 (* 10^n )
      if ( n != 3 * m ) 
         min1 = min1 * 2.0
         max1 = max1 * 2.0
         min2 = min1
         max2 = max1
         int1 = int1 * 2.0
      endif
      if ( n = 3 * m ) 
         min2 = max1
         min1 = min1 * 2.5
         max1 = max1 * 2.5
         max2 = max1
         int1 = int1 * 2.5
         m    = m + 1
      endif
      n  = n + 1
   endwhile
*** smaller and smaller
else
   int1 = 0.2
   min1 = 4
   max1 = 8
   min2 = min1
   max2 = max1
   flag = 1
   n    = 1
   m    = 1
   while (flag)
      if ( range >= min2 & range < max2 ) 
         int = int1
         flag = 0
      endif
* cint is 1 , 2, or 5 (* 10^(-n) )
      if ( n != 3 * m ) 
         min1 = min1 * 0.5
         max1 = max1 * 0.5
         int1 = int1 * 0.5
         min2 = min1
         max2 = max1
      endif
      if ( n = 3 * m )
         max2 = min1
         min1 = min1 * 0.4
         max1 = max1 * 0.4
         int1 = int1 * 0.4
         min2 = min1
         m    = m + 1
      endif
      n = n + 1
   endwhile
endif

say 'cint = 'int
return int




