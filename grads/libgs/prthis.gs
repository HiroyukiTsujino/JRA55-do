function prthis (args)
   printer = subwrd(args,1)
   if (printer = '') ; printer = 2 ; endif
   opt1 = subwrd(args,2)
   opt2 = subwrd(args,3)
   'enable print /short/f/nakano/tmp.gx'
   'print'
   'disable print'
   '!gxps 'opt1' 'opt2' -i /short/f/nakano/tmp.gx -o /short/f/nakano/tmp.ps ;lpr -P'printer' /short/f/nakano/tmp.ps &'
return
