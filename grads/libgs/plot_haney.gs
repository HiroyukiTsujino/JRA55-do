function haney (args)
  'open ta_ast'
  'open dq_dt'

  pos.1 = 5; pos.2 = 6; 
  pos.3 = 3; pos.4 = 4;
  pos.5 = 1; pos.6 = 2;

  monname.1 = 'Jan.'; monname.2 = 'Feb.'; monname.3 = 'Mar.';
  monname.4 = 'Apr.'; monname.5 = 'May' ; monname.6 = 'Jun.';
  monname.7 = 'Jul.'; monname.8 = 'Aug.'; monname.9 = 'Sep.';
  monname.10= 'Oct.'; monname.11= 'Nov.'; monname.12= 'Dec.';

  'set font 1'
  'set lon 23 383'

  waittime = 600
* interval time for printing figures [sec]

  it = 13
  while (it <= 12)
    it2 = it
    if (it > 6); it2 = it2 - 6 ; endif   
    'set t 'it
    'run subp p 3 2 'pos.it2
    'set grads off'
    'set grid off'
    'run fineshade_kwmy taast -30 35'
    'run cbar_fine -25 -20 -15 -10 -5 0 5 10 15 20 25 30 35'
    'set gxout contour'
    'set ccolor 1'
    'set cint 5'
    'd taast'
    'draw title Apparent equilibrium air temperature [`3.`1C]\'monname.it
    if ((it = 6) | (it = 12))
      'run prthis p2 -c '
      '!sleep 'waittime
*      say 'Input anything.'
*      pull nothing
      'c'
    endif
    it = it + 1
  endwhile

  it = 1
  while (it <= 12)
    it2 = it
    if (it > 6); it2 = it2 - 6 ; endif   
    'set t 'it
    'run subp p 3 2 'pos.it2
    'set grads off'
    'set grid off'
    'set cmax 150'
    'set cmin   0'
*    'run fineshade_kwmy dqdt.2 0 60'
    'define gamma = 2368.88 / dqdt.2'
    'run fineshade_kwmy gamma 40 140'
    'run cbar_fine 40 50 60 70 80 90 100 110 120 130 140'
    'set gxout contour'
    'set ccolor 1'
*    'set cint 10'
    'set cint 20'
*    'd dqdt.2 '
    'set cmax 150'
    'set cmin   0'
    'set cint 10'
    'd gamma'
    'draw title restoring time for 50m (10`3.`1C, 35psu)[day]\'monname.it
    if ((it = 6) | (it = 12))
      'run prthis p2 -c '
*      '!sleep 'waittime
*      say 'Input anything.'
*      pull nothing
      'c'
    endif
    it = it + 1
  endwhile
  'quit'
return
end
