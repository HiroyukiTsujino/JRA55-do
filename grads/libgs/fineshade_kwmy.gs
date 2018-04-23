function fshade (args)
   varn = subwrd(args,1)
   cmi = subwrd(args,2)
   cma = subwrd(args,3)
   offset = 21
   ncol = 61-2 
   'run colortab /home/a/kwmy/lib/grads/grads'
   if (cmi = '')
      'set gxout stat'
      'd 'varn
      cmicma = sublin(result,8)
      cmi = subwrd(cmicma,4)
      cma = subwrd(cmicma,5)
   endif
   delta = (cma - cmi)/ncol
   i = 0 
   levs = ''
   cols = ''
   while (i <= ncol)
      levs = levs' 'cmi+delta*i
      cols = cols' 'offset+i 
      i = i + 1
   endwhile
   cols = cols' '20+i
   'set gxout shaded'
   'set clevs 'levs
   'set ccols 'cols
   'd 'varn
return
end
