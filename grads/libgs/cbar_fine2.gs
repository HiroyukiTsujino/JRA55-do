*
*  Script to plot a colorbar
*
*  The script will assume a colorbar is wanted even if there is 
*  not room -- it will plot on the side or the bottom if there is
*  room in either place, otherwise it will plot along the bottom and
*  overlay labels there if any.  This can be dealt with via 
*  the 'set parea' command.  In version 2 the default parea will
*  be changed, but we want to guarantee upward compatibility in
*  sub-releases.
*
function colorbar (args)
*
*  Check shading information
*
  i = 1
  tmpval = subwrd(args,1)
  while (tmpval != '')
     _val.i = tmpval
     i = i + 1
     tmpval = subwrd(args,i)
  endwhile
  _nvar = i - 1

  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None') 
    say 'Cannot plot color bar: No shading information'
    return
  endif
* 
*  Get plot size info
*
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo = subwrd(rec4,4)
  ylo = ylo - 0.2
  xhi = subwrd(rec3,6)
  xd = xsiz - xhi
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<0.6 & xd<1.0) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
  if (ylo<0.6 | xd>1.5)
    xl = xhi + xd/2 - 0.4
    xr = xl + 0.2
    xwid = 0.2
    ywid = 0.5
    if (ywid*cnum > ysiz*0.8) 
      ywid = ysiz*0.8/cnum
    endif
    ymid = ysiz/2
    yb = ymid - ywid*cnum/2
    'set string 1 l 5'
    vert = 1
  else
    ymid = ylo/2
    yt = ymid + 0.2
    yb = ymid
    xmid = xsiz/2
    xwid = 0.8
    if (xwid*cnum > xsiz*0.8)
      xwid = xsiz*0.8/cnum
    endif
    xl = xmid - xwid*cnum/2
    'set string 1 tc 5'
    vert = 0
  endif
*
*  Plot colorbar
*
  'set strsiz 0.12 0.13'
  num = 0
*  while (num<cnum) 
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    hiold = hi
    hi = subwrd(rec,3)
    'set line 'col
    if (vert) 
      yt = yb + ywid
    else 
      xr = xl + xwid
    endif
    'draw recf 'xl' 'yb' 'xr' 'yt
******* frame ************************
    'set line 1'
    'draw rec 'xl' 'yb' 'xr' 'yt
*************************************
    if (num<cnum-1)
      recnxt = sublin(shdinfo,num+3)
      hinxt = subwrd(recnxt,3)
      if (hinxt = '>')
          hinxt = hi + (hi - hiold) 
      endif
      rc = match(hi,hinxt,vert,xr,yt,yb,xwid,ywid)
     if (rc)
        'draw string '_xstr' '_ystr' '_val._nhit
      endif
    endif
    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
return
end

function match(hi,hinxt,vert,xr,yt,yb,xwid,ywid)
   rc = 0
   i = 1
   while (i <= _nvar)
      if ((hi <= _val.i) & (_val.i < hinxt))
         _nhit = i
         if (vert)
            _xstr = xr + 0.05
            _ystr = yt + (_val.i - hi) / (hinxt - hi) * ywid
         else
            _xstr = xr + (_val.i - hi) / (hinxt - hi) * xwid
            _ystr = yb - 0.05
         endif
         rc = 1
         i = _nvar + 1
      endif
      i = i + 1
   endwhile
return rc
