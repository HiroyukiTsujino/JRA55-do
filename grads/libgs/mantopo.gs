#! grads -lc
###############################################################
function mantopo(args)
#
#  Manipulation Tool for Ocean Topography Data
#                             --- written by S.Yukimoto ---
#  version 0.1  ---  2004/02/20
#  version 0.2  ---  2004/05/05
#  version 1.0  ---  2006/03/08
#  - cgcm3.1.1用公開版
#  - 選択したポイントで、深さ入力のプロンプトに対し'r'と入力す
#    ると、リファレンス・データに置き換えられるようにした。プロ
#    グラム起動時にリファレンス・データのファイル名(optional)を
#    入力する（デフォルトは元データinctl）。
#  version 1.1  ---  2006/09/20
#  - 変更履歴をファイルに出力し、それを読み込んで反映させること
#    ができるようにした。
#  - 矩形領域を選択し、一括で値を変更できるようにした。
#  - カラーバーを明度変化を優先させていたのを色相変化が優先する
#    ように変更した。元に戻すには、function setlevsのflpri=0を
#    flpri=1に変更する。
#  - シグマ層の数ksgmを入力し、カラー・レベルをシグマ層より深い
#    レベルだけで変化させる。
#  version 1.2  ---  2012/8/16 (Nakano)
#  - 全球を描くときは時間短縮のため gxout contourとする。
#  - select domain の場合に描画範囲を全球に戻さない。
###############################################################
#******************
#* 初期設定
#******************
'reinit'

#* 入力ファイル名
inctl=subwrd(args,1)
if(args='')
  prompt '> Input original topog data ctl-file = '
  pull inctl
endif
prompt '> Input reference topog data ctl-file (optional) = '
pull refctl
if(refctl='')
  refctl=inctl
endif

#* 出力ファイル名（デフォルト）
_fname='mantopo.gd'
_outctl='mantopo.ctl'
_outlog='mantopo.log'

#* 地図データセット
*'set mpdset map_trsfm/lowres.tr'
'set mpdset map_trsfm/hires.tr'
'set poli off'

#* ファイルオープン
'open 'inctl
'set grads off'
'open 'refctl
dummy=writelog('start',0)

#* 以下の情報を取得
#  _inctl      : 入力データctlファイル
#  _indata     : 入力データファイル
#  _xsiz       : 格子数 x方向
#  _ysiz       : 格子数 y方向
#  _zsiz       : 鉛直レベル数
#  _lon0,_lat0 : 領域範囲 (LON0,LAT0)-(LON1,LAT1)
#  _lon1,_lat1 :
#  _levls      : 鉛直レベルのリスト
getinfo()
depmax=subwrd(_levls,_zsiz)

# 全領域表示範囲
_xs=-0.01*_xsiz
_xe=_xsiz*1.01

#* レベルのカラー設定
setlevs()

#* のりしろは除いて変数にセット
if (_lon1-_lon0>360)
  'set x 3 '%(_xsiz-2)
else
  'set x 2 '%(_xsiz-1)
endif
'set y 1 '_ysiz
'define dptmp=dp*0.01' ;* 深さ [cm] -> [m]
'define lvtmp=lv'

#* 参考値データ
if(refctl!='')
* xoff=-2 ; yoff=-1 ;* 座標オフセット
  rc=getoffset()
  xoff=subwrd(rc,1)
  yoff=subwrd(rc,2)
  'define rfdp=dp.2*0.01'
  'define rflv=lv.2'
else
  xoff=0 ; yoff=0
  'define rfdp=dptmp'
  'define rflv=-1'
endif

#* その他各種初期設定
# デフォルト選択領域
*  _lon='270 390'
*  _lat='30 90'
  _lon=_lon0' '_lon1
  _lat=_lat0' '_lat1
  _long=_lon
  _latg=_lat
  px0=subwrd(_lon,1)
  px1=subwrd(_lon,2)
# 文字サイズ
 _mindsz=0.03 ; _fcdsz=1.2
 _digsiz=_mindsz+_fcdsz/(px1-px0)
 'set strsiz 0.06'
 'set string 1 r 1'

# 表示設定
 'set mproj scaled'
 'set parea 0.75 9.75 1.0 7.5'

#* フラグ等初期化
_flgdp=1  ;* 深さ画面
_flglv=0  ;* レベル画面
_flsel=1  ;* 全領域/選択領域
_fsav=1   ;* ファイル出力済
_npr=0    ;* プリントページ数
_flarge=1 ;* 大領域
event=0

#* ボタン初期設定
intbtn()
drwbtn()
'redraw button 1 '_flgdp

#* 初期画面の領域選択
setdom(_xs,_xe,_flsel)
#_lon='0 360'
#_lat='-90 90'

#* 深さプロット（初期画面）
plotdp(_flgdp,_flarge,0)

#* 選択領域の座標
'q gxinfo'
rec=sublin(result,3)
xlo=subwrd(rec,4) ; xhi=subwrd(rec,6)
rec=sublin(result,4)
ylo=subwrd(rec,4) ; yhi=subwrd(rec,6)
'set line 0'

#******************
#* ループスタート
#******************

while(1)

if(_flsel=0)
 say '> Select a domain ...'
else
 say '> Click (or click&drag) to modify the Depth ...'
endif

# ボタン描画
drwbtn()
'redraw button 2 '_flglv
'redraw button 1 '_flgdp
'redraw button 4 '_fsav
'redraw button 6 0'
'redraw button 7 '%(1-_flsel)
'draw dropmenu '_dmenu

# イベント取得
'set rband 11 box 'xlo' 'ylo' 'xhi' 'yhi
'q pos'
wtyp=subwrd(result,6)
nev=subwrd(result,7)
ksel=subwrd(result,8)

#* クリックした座標が地図の中 *****************
if (wtyp=2)
  if (_flsel=0) ; continue ; endif
#  モデル格子座標に変換
  pxs=subwrd(result,3)
  pys=subwrd(result,4)
  pxe=subwrd(result,8)
  pye=subwrd(result,9)
  'q xy2gr 'pxs' 'pys
  xgs=subwrd(result,3)
  ygs=subwrd(result,6)
  'q xy2gr 'pxe' 'pye
  xge=subwrd(result,3)
  yge=subwrd(result,6)
  ixgs=math_int((xgs-0.5))+1
  iygs=math_int((ygs-0.5))+1
  ixge=math_int((xge-0.5))+1
  iyge=math_int((yge-0.5))+1
  if (ixgs>ixge) ; tmp=ixgs ; ixgs=ixge ; ixge=tmp ; endif
  if (iygs>iyge) ; tmp=iygs ; iygs=iyge ; iyge=tmp ; endif
* say 'ixgp = 'ixgs' - 'ixge
* say 'iygp = 'iygs' - 'iyge
*     if(xgp>0.5)
*       xgp=xgp+0.5
*     else
*       xgp=xgp-0.5
*     endif
*     ygp=ygp+0.5
  'set x '_xs' '_xe
  'set y 1 '_ysiz
  ii=0
  dpmean=0
  dpmin=99999 ; dpmax=-999
  ygp=iyge ; while(ygp>=iygs)
    xgp=ixgs ; while(xgp<=ixge)
      'q defval dptmp 'xgp' 'ygp
# 更新前の値
      dptpi=subwrd(result,3)
# 参考値
      rfxgp=xgp+xoff
      rfygp=ygp+yoff
      'q defval rfdp 'rfxgp' 'rfygp
      dprfi=subwrd(result,3)
      if (dptpi>-1)
        ii=ii+1
        dptp.ii=dptpi
        dprf.ii=dprfi
        xgtp.ii=xgp
        ygtp.ii=ygp
*       say ' xgp = 'xgtp.ii'  ygp = 'ygtp.ii'   dptp.'ii' = 'dptp.ii
        dpmean=dpmean+dptpi
        if (dptpi>dpmax) ; dpmax=dptpi ; endif
        if (dptpi<dpmin) ; dpmin=dptpi ; endif
*       'q defval rflv 'rfxgp' 'rfygp
*       fr_rf=subwrd(result,3)
# 選択した点を UNDEF にする
        'set defval dptmp 'xgp' 'ygp' -999'
        'set defval lvtmp 'xgp' 'ygp' -999'
      endif
    xgp=xgp+1 ; endwhile
  ygp=ygp-1 ; endwhile
  cntp=ii
  if (cntp>0) 
    dpmean=dpmean/cntp 
  else
    dpmean=-999 
    dpmax=-999 
    dpmin=-999 
  endif
# 再描画
  plotdp(_flgdp,_flarge,0)
# 更新値を入力
  if (ixgs<ixge) ; ranix=ixgs'-'ixge ; else ; ranix=ixgs ; endif
  if (iygs<iyge) ; raniy=iygs'-'iyge ; else ; raniy=iygs ; endif
  say 'depth ('ranix','raniy') : mean = 'dpmean'  min = 'dpmin'   max = 'dpmax
  prompt '>>  modify depth ---> '
  pull pdepin
  ii=1 ; while(ii<=cntp)
    if(pdepin='')
      pdep=dptp.ii
    else
      if (pdepin='r')
        pdep=dprf.ii
      else
        pdep=pdepin
      endif
    endif
    xgp=xgtp.ii
    ygp=ygtp.ii
    if(pdep>depmax) ; pdep=depmax ; endif
    plev=getplev(pdep)
*   say 'plev = 'plev
    'set x '_xs' '_xe
    'set y 1 '_ysiz
#   更新値をセット
    'set defval dptmp 'xgp' 'ygp' 'pdep
    'set defval lvtmp 'xgp' 'ygp' 'plev
#   更新情報をファイルに出力
    if (pdep != dptp.ii)
      reclog='> 'xgp' 'ygp' 'dptp.ii' > 'pdep'  'plev
      rc=write(_outlog,reclog,append)
    endif
#   サイクリック条件のセット
    if(xgp>_xsiz+1.0)
      xgp2=xgp-_xsiz
      'set defval dptmp 'xgp2' 'ygp' 'pdep
      'set defval lvtmp 'xgp2' 'ygp' 'plev
    else
      if(xgp<0.0)
        xgp2=xgp+_xsiz+1
        'set defval dptmp 'xgp2' 'ygp' 'pdep
        'set defval lvtmp 'xgp2' 'ygp' 'plev
      endif
    endif
    if(xgp>=0.0 & xgp<=_xe-_xsiz)
      xgp2=xgp+_xsiz
      'set defval dptmp 'xgp2' 'ygp' 'pdep
      'set defval lvtmp 'xgp2' 'ygp' 'plev
    endif
    if(xgp<_xsiz+1.0 & xgp>=_xs+_xsiz)
      xgp2=xgp-_xsiz-1
      'set defval dptmp 'xgp2' 'ygp' 'pdep
      'set defval lvtmp 'xgp2' 'ygp' 'plev
    endif
  ii=ii+1 ; endwhile
#
# 再描画
  plotdp(_flgdp,_flarge,0)
# 更新後未保存
  _fsav=0
#

#* ボタンがクリックされた場合 *****************
else

# イベント処理
  if (wtyp=1)
    rtnc=procevt(nev)
  endif
  if (wtyp=3) 
    rtnc=procev3(nev,ksel)
  endif
  if (rtnc=9)
    rc=close(_outlog)
    'quit'
  endif
endif
endwhile

#******************
#* ループ終わり
#******************
 
###############################################################
# subroutines
###############################################################

function getplev(pdep)
levstmp='0 '_levls
k=_zsiz ; while(k>=1)
  kdep=subwrd(levstmp,k)
# if(pdep*100>kdep+0.01) ; return k ; endif
  if(pdep>kdep+0.01) ; return k ; endif
k=k-1 ; endwhile
return 0

###############################################################

function getoffset()
 'set dfile 1' ; 'q dims'
 tmp1=sublin(result,2) ; x1=subwrd(tmp1,11)
 tmp2=sublin(result,3) ; y1=subwrd(tmp2,11)
 'set dfile 2' ; 'q dims'
 tmp1=sublin(result,2) ; x2=subwrd(tmp1,11)
 tmp2=sublin(result,3) ; y2=subwrd(tmp2,11)
 xoff=x2-x1
 yoff=y2-y1
return xoff' 'yoff

###############################################################

function plotdp(flgdp,flarge,ksel)
#
# 深さ／レベル　プロット
#  
'set lon '_lon
'set lat '_lat
'clear graphics'
'set grads off'
'set map 0 1 1'
'set grid off'
if(flgdp=1)
 if (_lon=_long & _lat=_latg)
   'set gxout contour'
 else
   'set gxout grfill'
 endif
 'set clevs '_clvdp
 'set ccols '_ccldp
 'd dptmp-0.01'
 colorbar()
 if(flarge=0)
   'set gxout grid'
   'set digsize '_digsiz
   'set ccolor 1 '
   'd dptmp'
 endif
else
 if (_lon=_long & _lat=_latg)
   'set gxout contour'
 else
   'set gxout grfill'
 endif
 if(ksel=0)
   'set clevs '_clvlv
   'set ccols '_ccllv
 else
   'set clevs 'ksel
   'set ccols 96 95 '
 endif
 'd lvtmp'
 colorbar()
 if(flarge=0)
   'set gxout grid'
   'set digsize '_digsiz
   'set ccolor 1 '
   'd lvtmp'
 endif
endif
'set strsiz 0.10'
'set string 1 l 4'
'draw string 0.5 0.35 ORIGINAL='_indata
if (_fsav)
  'draw string 0.5 0.15 MODFIED='_fname
endif
return

###############################################################

function setlevs()
#
#* 鉛直レベルのカラー設定
#
'set rgb 95    0  10  80' ;* 海の色（SEL_LEVEL表示）
'set rgb 96  225 160  70' ;* 陸の色
nrgbmx=76 ;* 設定可能な色数上限値

# 最も浅いレベルを入力（普通はシグマ層数）
prompt '> Enter the shallowest level (ex. # of sigma layers) ... ksgm = '
pull ksgm
if (ksgm='') ; ksgm=1 ; endif
if (ksgm<1) ; ksgm=1 ; endif
if (ksgm>_zsiz) ; ksgm=_zsiz ; endif

zmax=_zsiz-ksgm+1
#zmax=64
#
 flpri=0 ;# 色相変化を優先 
#flpri=1 ;# 明度変化を優先 
#*** 明度変化を優先の場合
if (flpri=1)
# ncb*2: 明度の諧調数
 ncb=4
# ncph 色相の諧調数
 if(zmax>nrgbmx)
   ncph=nrgbmx/(ncb*2)
 else
   ncph=(zmax-1)/(ncb*2)
 endif
 ncph=math_int(ncph)+1
#*** 色相変化を優先の場合
else
# ncph 色相の諧調数
 if (zmax>24)
   ncph=8
 else
   ncph=6
 endif
# ncb*2: 明度の諧調数
 if(zmax>nrgbmx)
   ncb=nrgbmx/ncph
 else
   ncb=(zmax-1)/ncph/2
 endif
 ncb=math_int(ncb)+1
 if (ncb<1) ; ncb=1 ; endif
endif
# 色相の変化幅
 dphi=-210/ncph
# ncmax: 色諧調の総数
 ncmax=ncph*ncb*2
 if (ncmax>nrgbmx)
   ncb = 4 ; ncph=9 ; ncmax=nrgbmx
 endif
# 2次元カラーテーブル設定
*say 'rgb2d 'ncph' 'ncb'  270  'dphi
*'rgb2d 'ncph' 'ncb'  270  'dphi
rc=rgb2d(ncph,ncb,270,dphi)

if(zmax<ncmax)
 ks=1
else
 ks=zmax-ncmax+2
endif
ke=zmax
ncol=ke-ks+2
nc=96 ;* 陸の色
_clvdp='0'
_clvlv='0'
_ccldp=nc
_ccllv=nc
# ここから海の色
nc=15+ncol   ;* k=keが最も暗い色
#nc=15+ncmax ;* k=ksが最も明るい色
ni=-1 ;* color increment
k=ks ; while(k<=ke)
  nc=nc+ni
  kl=k+ksgm-1
  zl=subwrd(_levls,kl)
* _clvdp=_clvdp' 'zl/100
* kr=ke-k+1
* zl=subwrd(_levls,kr)
  _clvdp=_clvdp' 'zl
  _ccldp=_ccldp' 'nc
  _clvlv=_clvlv' 'k
  _ccllv=_ccllv' 'nc
k=k+1 ; endwhile
_ccldp=_ccldp' 0'
_ccllv=_ccllv' 0'

return

###############################################################

function intbtn()

# ボタン初期設定

'set rgb 97 100 100 100'
'set rgb 98  75  75  75'
'set rgb 99 180 180 180'
'set button 0 97 98 99 3 97 99 98 12'
'set dropmenu 0 97 98 99 1 97 99 98 1 97 98 99 99 98 6'
_nevent=8
txt.1='DEPTH'      ; xbc.1=4.0  ; dx.1=0.985
txt.2='LEVEL'      ; xbc.2=5.0  ; dx.2=0.985
txt.3='READ LOG'   ; xbc.3=7.3  ; dx.3=0.885
txt.4='SAVE'       ; xbc.4=8.2  ; dx.4=0.885
txt.5='PRINT'      ; xbc.5=9.1  ; dx.5=0.885
txt.6='SEL DOMAIN' ; xbc.6=1.0  ; dx.6=1.185
txt.7='ALL DOMAIN' ; xbc.7=2.2  ; dx.7=1.185
txt.8='QUIT'       ; xbc.8=10.0 ; dx.8=0.885
txt.9='SEL LEVEL'  ; xbc.9=6.0  ; dx.9=0.985
ybc=8.2
dx=0.885 ; dy=0.3
dxh=dx*0.5 ;  dyh=dy*0.5
n=1
while(n<=_nevent)
  _btn.n=n' 'xbc.n' 'ybc' 'dx.n' 'dy' 'txt.n
  _xb0.n=xbc.n-dxh
  _xb1.n=xbc.n+dxh
n=n+1 ; endwhile

# Dropmenu
n=_nevent+1
menulist=txt.n
k=1 ; while(k<=_zsiz-1)
  menulist=menulist' | K = 'k
k=k+1 ; endwhile
menulist=menulist' '
_dmenu=n' 'xbc.n' 'ybc' 'dx.n' 'dy' 'menulist
_yb0=ybc-dyh
_yb1=ybc+dyh
return
#
###############################################################

function drwbtn()
  n=1
  while(n<=_nevent)
    'draw button '_btn.n
  n=n+1 ; endwhile
  'draw dropmenu '_dmenu
return

###############################################################

function procevt(event)
#
#* イベント処理
#
  if(event=1|event=2) ;*** 表示スイッチ [DEPTH]/[LEVEL]
    if(event=1)
      _flglv=0
      _flgdp=1
      'redraw button 2 '_flglv
      'redraw button 1 '_flgdp
    else
      _flglv=1
      _flgdp=0
      'redraw button 2 '_flglv
      'redraw button 1 '_flgdp
    endif
    plotdp(_flgdp,_flarge,0)
    return event
  else
    if(event=3) ;*** ログファイルを読み込んで反映
      'redraw button 3 1'
      rc=readlog()
      plotdp(_flgdp,_flarge,0)
      return event
    endif
    if(event=4) ;*** ファイル保存
      'redraw button 4 1'
      dptout()
      _fsav=1 
      dummy=writelog('saved',1)
      return event
    endif
    if(event=5) ;*** 印刷
      _npr=_npr+1
      dummy=print(_npr)
      say dummy
      return event
    endif
    if(event=6|event=7) ;*** 領域変更
      if(event=6)
        _flsel=1
        setdom(_xs,_xe,_flsel)
        'redraw button 6 0'
        'redraw button 7 0'
      else
        _flsel=0
        setdom(_xs,_xe,_flsel)
        'redraw button 7 1'
        'redraw button 6 0'
      endif
      plotdp(_flgdp,_flarge,0)
      return event
    endif
    if(event=8) ;*** 終了
      'redraw button 8 1'
      rtn=finalize()
      if(rtn!=9) ; 'redraw button 8 0' ; endif
      dummy=writelog('end',1)
      return rtn
    endif
  endif
return 0
###############################################################

function procev3(event,ksel)
#
#* メニュー・イベント処理
#
if (event=9)
  _flglv=1
  _flgdp=0
  plotdp(_flgdp,_flarge,ksel)
endif
return 0

###############################################################

function dptout()
#
#* データファイル出力
#
while(1)
  prompt '> Enter output TOPOG file = ('_fname') ? '
  pull tmp
  if(tmp!='')
    _fname=tmp
    break
  endif
  if(_fname!='')
    break
  endif
endwhile
'set gxout fwrite'
'set fwrite -be '_fname
'set x 1 '_xsiz
'set y 1 '_ysiz
'd dptmp*100'  ;* [m] -> [cm]
'd lvtmp'
'disable fwrite'
say ' >>> output data file : '_fname
#
#* ctlファイル作成（入力ctlを下敷きに）
#
  fltmp='mantopo.tmp'
  '!basename '_fname' .gd > 'fltmp
  buff=read(fltmp)
  stat=sublin(buff,1)
  basename=sublin(buff,2)
  if (stat=0)
    _outctl=basename'.ctl'
  endif
  fvar=0
  n=0 ; while(n<=1000)
    buf=read(_inctl)
    rc=sublin(buf,1)
    if(rc=2) ; break ; endif
    n=n+1
    rec=sublin(buf,2)
    dsc=subwrd(rec,1)
    if(dsc='ENDVARS') ; fvar=0 ; endif
    if(dsc='DSET'|dsc='FILEHEADER'|fvar=1)
      if (dsc='DSET') 
        rec='DSET  ^'_fname
        rc=wrtrec(_outctl,rec,n)
      endif
      if (fvar=1)
        part1=subwrd(rec,1)'  0  99  '
        part2=subwrd(rec,4)' 'subwrd(rec,5)' 'subwrd(rec,6)
        rec=part1' 'part2
        rc=wrtrec(_outctl,rec,n)
      endif
    else 
      rc=wrtrec(_outctl,rec,n)
    endif
    if(dsc='VARS') ; fvar=1 ; endif
  endwhile
  rc=close(_outctl)
say '            ctl-file : '_outctl
return
###############################################################

function wrtrec(file,rec,nrec)
  if(nrec=1) 
    rc=write(file,rec)
  else 
   rc=write(file,rec,append) 
  endif
return

###############################################################

function readlog(title,iappnd)
  prompt 'Enter the log-file ... '
  pull finlog
  while(1)
    buff=read(finlog)
    stat=sublin(buff,1)
    if (stat=0)
      rec=sublin(buff,2)
      hd=subwrd(rec,1)
      if (hd='>')
        xgp=subwrd(rec,2)
        ygp=subwrd(rec,3)
        pdep=subwrd(rec,6)
        plev=subwrd(rec,7)
        'set defval dptmp 'xgp' 'ygp' 'pdep
        say 'set defval dptmp 'xgp' 'ygp' 'pdep
        'set defval lvtmp 'xgp' 'ygp' 'plev
        say 'set defval lvtmp 'xgp' 'ygp' 'plev
      endif
    else
      break
    endif
  endwhile
return

###############################################################

function writelog(title,iappnd)
  fltmp='mantopo.tmp'
  '!date > 'fltmp
  buff=read(fltmp)
  stat=sublin(buff,1)
  if (stat=0)
    date=sublin(buff,2)
    rec='*** mantopo 'title': 'date
  else
    say 'writelog : stat = 'stat
    rec='*** mantopo 'title': ?????'
  endif
  if (iappnd=1)
    rc=write(_outlog,rec,append)
  else
    rc=write(_outlog,rec)
  endif
  buff=close(fltmp)
return

###############################################################

function setdom(xs,xe,flsel)
#* 領域設定
# 全領域にセット
if(flsel=0)
  _lon=_lon0' '_lon1
  _lat=_lat0' '_lat1
  'set lon '_lon
  'set lat '_lat
  _flarge=1
  return
endif
# 領域をマウスで選択
'clear graphics'
'redraw button 6 1'
say '> Select Domain by mouse dragging ...'
*'set x 'xs' 'xe
*'set y -1 '%(_ysiz+2)
if (_lon=_long & _lat=_latg)
  'set gxout contour'
else
  'set gxout grfill'
endif
'set clevs '_clvdp
'set ccols '_ccldp
'd dptmp-0.01'
'q gxinfo'
rec=sublin(result,3)
xlo=subwrd(rec,4) ; xhi=subwrd(rec,6)
rec=sublin(result,4)
ylo=subwrd(rec,4) ; yhi=subwrd(rec,6)
'set rband 21 box 'xlo' 'ylo' 'xhi' 'yhi
'q pos'
*say result
wtyp=subwrd(result,6)
if(wtyp=2)
  xa0=subwrd(result,3)
  ya0=subwrd(result,4)
  xa1=subwrd(result,8)
  ya1=subwrd(result,9)
  'q xy2w 'xa0' 'ya0
  xa0=subwrd(result,3)
  ya0=subwrd(result,6)
  'q xy2w 'xa1' 'ya1
  xa1=subwrd(result,3)
  ya1=subwrd(result,6)
  if(xa0>xa1)
   tmp=xa0 ; xa0=xa1 ; xa1=tmp
  endif
  if(ya0>ya1)
   tmp=ya0 ; ya0=ya1 ; ya1=tmp
  endif
  slon=xa0' 'xa1
  slat=ya0' 'ya1
* say 'LON='slon
* say 'LAT='slat
  px0=subwrd(slon,1)
  px1=subwrd(slon,2)
 'redraw button 6 0'
else
 'redraw button 6 0'
  return
endif
#  文字の大きさ
if (px1-px0>10.)
  _digsiz=_mindsz+_fcdsz/(px1-px0)
else
  _digsiz=_mindsz+_fcdsz*0.1
endif
'set lon 'slon
'set lat 'slat
'q dim'; xinfo=sublin(result,2)
tmp=subwrd(xinfo,3)
if(tmp='varying')
  x0=subwrd(xinfo,11)
  x1=subwrd(xinfo,13)
  if(x1<x0+1.1);* 幅が小さすぎる時元に戻す
    'set lon '_lon
    'set lat '_lat
    return
  endif
# 大領域かどうか
  if(x1-x0>40)
    _flarge=1
  else
    _flarge=0
  endif
  _lon=slon
  _lat=slat
  'set lon '_lon
  'set lat '_lat
endif
return

###############################################################

function print(npr)
file='mantopo.tmp'npr
'enable print 'file'.gm'
'print'
'disable print'
'!gxeps -c -i 'file'.gm -o 'file'.eps'
say 'output PostScript file 'file'.eps'
return result

###############################################################

function finalize()
if(_fsav=0)
  say 'No save after modification'
  prompt ' Quit anyway ... OK ? ' ; pull ans
else
  ans='y'
  say 'Quitting ... '
endif
if ( ans='y' )
 rtn=9
else
 rtn=4
endif
return rtn
###############################################################

function getinfo()
#* データの各種情報取得
'q file'
# 入力データファイル
rec=sublin(result,2)
_inctl=subwrd(rec,2)
rec=sublin(result,3)
_indata=subwrd(rec,2)
# 格子数
rec=sublin(result,5)
_xsiz=subwrd(rec,3)
_ysiz=subwrd(rec,6)
_zsiz=subwrd(rec,9)
'q dim'
# 全領域 LON , LAT
rec=sublin(result,2)
_lon0=subwrd(rec,6)
_lon1=subwrd(rec,8)
rec=sublin(result,3)
_lat0=subwrd(rec,6)
_lat1=subwrd(rec,8)
# 鉛直レベルのリスト
_levls=''
k=1 ; while(k<=_zsiz)
  'set z 'k
  dpl=subwrd(result,4)
  _levls=_levls' 'dpl
k=k+1 ; endwhile
*say 'x size ='_xsiz
*say 'y size ='_ysiz
*say 'z size ='_zsiz
 say 'levels ='_levls
return

###############################################################

function colorbar (args)
#
#  Check shading information
#
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None') 
    say 'Cannot plot color bar: No shading information'
    return
  endif
# 
#  Get plot size info
#
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo = subwrd(rec4,4)
  xhi = subwrd(rec3,6)
  xd = xsiz - xhi
#
#  Decide if horizontal or vertical color bar
#  and set up constants.
#
  if (ylo<0.6 & xd<1.0) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
  xl = xhi + xd/2 - 0.4
  xr = xl + 0.2
  xwid = 0.2
  ywid = 0.5
  if (ywid*cnum > ysiz*0.8) 
    ywid = ysiz*0.8/cnum
  endif
  ymid = ysiz/2
  yt = ymid + ywid*cnum/2
  'set string 1 l 5'
  vert = 1
#
#  Plot colorbar
#
  'set strsiz 0.07 0.09'
  num = 0
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    hi = subwrd(rec,3)
    'set line 'col
    yb = yt - ywid
    'draw recf 'xl' 'yb' 'xr' 'yt
    if (num<cnum-1)
      'draw string '%(xr+0.05)%' 'yb' 'hi
    endif
    num = num + 1
    yt = yb
  endwhile

###############################################################

function rgb2d(nphi,nbeta,ph_sta,dphi)

#************************************************
# 2次元（色相 x 明度）の色テーブルを作成する
#                      2004/5/5 S.Yukimoto
#************************************************

#** パラメタのデフォルト設定 ***
# 色相  (Red=0, Green=120, Blue=240) [degrees]
# 色相の数
#   nphi=8
# 明度の数 = nbeta*2
#   nbeta=5
# 色相開始値 [degrees]
#   ph_sta=270
# 色相変化幅 [degrees]
#   dphi=-45
# ハイライト係数
    hilit=0.9

#** パラメタ入力値 ***
say 'rgb2d: nphi='nphi'  nbeta='nbeta'  ph_sta='ph_sta'  dphi='dphi

ncolx=nphi*nbeta*2
if(ncolx>84)
  say 'rgb2d error: number of colors is too big'
  exit
endif

#** set vivid colors ***
n=1 ; while(n<=nphi)
  phi=ph_sta+(n-1)*dphi
  phi=math_fmod(phi+360,360)
  ph0=phi/60
  if(phi>=0 & phi<120)
    red=2-ph0
    gre=ph0
    blu=0
  endif
  if(phi>=120 & phi<240)
    red=0
    gre=4-ph0
    blu=ph0-2
  endif
  if(phi>=240 & phi<=360)
    red=ph0-4
    gre=0
    blu=6-ph0
  endif
  red_v.n=scale(red)
  gre_v.n=scale(gre)
  blu_v.n=scale(blu)
  nn=math_format('%4.0f',n)
  phi=math_format('%4.0f',phi)
  red=math_format('%4.0f',red_v.n)
  gre=math_format('%4.0f',gre_v.n)
  blu=math_format('%4.0f',blu_v.n)
  nc=15+(n-1)*(nbeta*2)+nbeta+1
* say nc'  'red'  'gre'  'blu'   ph='phi
  'set rgb 'nc' 'red' 'gre' 'blu
n=n+1 ; endwhile

#** set dark colors ***
m=1 ; while(m<=nbeta)
* fct=1-m/(nbeta+1)
  tm=m/(nbeta+1.7)
  fct=1-tm
* say '*** dark *** 'm'  fct = 'fct
  n=1 ; while(n<=nphi)
    red=red_v.n*fct
    gre=gre_v.n*fct
    blu=blu_v.n*fct
    red=limit(red)
    gre=limit(gre)
    blu=limit(blu)
    nn=math_format('%4.0f',n)
    red=math_format('%4.0f',red)
    gre=math_format('%4.0f',gre)
    blu=math_format('%4.0f',blu)
    nc=15+(n-1)*(nbeta*2)+nbeta+1-m
*   say nc'  'red'  'gre'  'blu
    'set rgb 'nc' 'red' 'gre' 'blu
  n=n+1 ; endwhile
m=m+1 ; endwhile

#** set light colors ***
m=1 ; while(m<=nbeta-1)
  fct=m/(nbeta+1)*hilit
* say '*** light *** 'm'  fct = 'fct
  n=1 ; while(n<=nphi)
    red=(1-fct)*red_v.n+255*fct
    gre=(1-fct)*gre_v.n+255*fct
    blu=(1-fct)*blu_v.n+255*fct
    red=limit(red)
    gre=limit(gre)
    blu=limit(blu)
    nn=math_format('%4.0f',n)
    red=math_format('%4.0f',red)
    gre=math_format('%4.0f',gre)
    blu=math_format('%4.0f',blu)
    nc=15+(n-1)*(nbeta*2)+nbeta+1+m
*   say nc'  'red'  'gre'  'blu
    'set rgb 'nc' 'red' 'gre' 'blu
  n=n+1 ; endwhile
m=m+1 ; endwhile

* check
*ylo=4.0 ; yhi=5.0
*x0=0.5 ; dx=10/ncolx
*n=1; while(n<=ncolx)
*  nc=15+n
*  'set line 'nc
*  xlo=x0+(n-1)*dx ; xhi=xlo+dx
*  'draw recf 'xlo' 'ylo' 'xhi' 'yhi
*n=n+1 ; endwhile
*exit

return ncolx


function scale(arg)
  if(arg<0)
    rc=0
  else
    rc=arg*255
  endif
  rc=math_nint(rc)
  if(rc>255)
    rc=255
  endif
return rc

function limit(arg)
  rc=arg
  if(rc<0)
    rc=0
  endif
  if(rc>255)
    rc=255
  endif
return rc
