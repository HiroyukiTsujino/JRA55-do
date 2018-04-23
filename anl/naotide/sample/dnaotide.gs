function dnaotide(args)


i=96+1
iend=i+24
iint=1


'colmap rb64'
*'colmap rbow60'
'open /work4/ksakamot/CORE2jpn/e054/jpn_ana/tideh'

'set lon 130.5 135.5'
'set lat 32.6 34.8'

while(i<=iend)
  'c'

  'set t 'i

  'set gxout shaded'
  'clevslin -100 100 5'
  'd h'

  'cbarn2 1 0 5.5 0.5 0 10'
  'title -noz naotide[cm]'
  'signature e054_jpn_ana'

  'set gxout contour'
  'set clab off'
  'clevslin -400 400 50'
  'd h'

*  'save'
  '!sleep 2'

  i=i+iint
endwhile

'close 1'

return
