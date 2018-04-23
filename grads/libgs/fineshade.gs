function fshade (args)
   varn = subwrd(args,1)
   cmi = subwrd(args,2)
   cma = subwrd(args,3)
   ncol = 60
   'run colortab grads_kwmy'
   if (cmi = '')
      'set gxout stat'
      'd 'varn
      cmicma = sublin(result,8)
      cmi = subwrd(cmicma,4)
      cma = subwrd(cmicma,5)
   endif
   delta = (cma - cmi)/(ncol+1)
   i = 1
   levs = ''
   cols = ''
   while (i <= ncol-1)
      levs = levs' 'cmi+delta*i
      cols = cols' '20+i 
      i = i + 1
   endwhile
   cols = cols' '20+i
   'set gxout shaded'
   'set clevs 'levs
   'set ccols 'cols
   'd 'varn
return
end