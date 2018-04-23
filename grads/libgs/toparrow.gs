function toparrow(args)
say 'Click near bottom of the screen to quit'
say 'input for the length of an arrow is necessary'
r0 = subwrd(args,1)
while (1)
'query bpos'
x0 = subwrd(result,3)
y0 = subwrd(result,4)
    if (y0<'0.5'); break; endif;
'query bpos'
x1 = subwrd(result,3)
y1 = subwrd(result,4)
    if (y1<'0.5'); break; endif;
'set line 1 1 100'
r2 = (x1-x0)*(x1-x0) + (y1-y0)*(y1-y0)
********* sqrt *********
*   r = sqrt(r2)
*************************
'define rr = 'r2
'set x  1'
'set y  1'
'set z  1'
'set t  1'
'd sqrt(rr)'
r = subwrd(result,4)
************************
x2 = r0 * (x0-x1) / r + x1
y2 = r0 * (y0-y1) / r + y1
sin15 = 0.258819
cos15 = 0.965926
x = x2 - x1
y = y2 - y1
 xt1 =   x * cos15 + y * sin15 + x1
 yt1 = - x * sin15 + y * cos15 + y1
 xt2 =   x * cos15 - y * sin15 + x1
 yt2 =   x * sin15 + y * cos15 + y1
'set line 1 1 1'
'draw polyf 'x1' 'y1' 'xt1' 'yt1' 'xt2' 'yt2' 'x1' 'y1
endwhile
return






