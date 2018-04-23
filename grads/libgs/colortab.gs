*
*  usage: ga-> run colortab filename
*         Note that the suffix ".gct" is implicit.
*
function colortab (args)
   offset = 20
   fname = subwrd(args,1)'.gct'

   rc = read(fname)

   while (sublin(rc,1) != 2)

      line = sublin(rc,2)

      nc = subwrd(line,1)
      nr = subwrd(line,2)
      ng = subwrd(line,3)
      nb = subwrd(line,4)
      nc = nc + offset
*      say nc' 'nr' 'ng' 'nb
      'set rgb 'nc' 'nr' 'ng' 'nb
      rc = read(fname)

   endwhile
   rc = close(fname)

return