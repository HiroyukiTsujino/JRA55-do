function xy(args)
val = subwrd(args,1)
tt = subwrd(args,2)
'set font 1'
'set grads off'
'open HISTORY1'
'open HISTORY3'
if (val='u;v') 
  'set dfile 1'
else (val='tem')
  'set dfile 2'
endif	
'set lat 0 72'
'set lon 0 30'
'set mpdraw off'
'set z 'tt
'd 'val
'q dim'
rec4=sublin(result,4)
level = subwrd(rec4,6)
'draw title 'level' m'
return

