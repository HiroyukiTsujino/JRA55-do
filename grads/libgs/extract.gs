nn = 1
nnmax = 100000
while (nn<=nnmax)
   line1 = sublin(result,nn)
   word1 = subwrd(line1,1)
   if ( word1 = 'Result')
     value1 = subwrd(line1, 4)
     say value1
     nn = nnmax + 1
   endif
   nn = nn + 1
endwhile

