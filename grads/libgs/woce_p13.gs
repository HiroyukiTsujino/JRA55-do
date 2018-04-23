*--------------- Potential Temperature  -------------------
'reinit'
'open hs_t.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=tt'
rc = wocep13(aa,1,0.5,5,2,1,20)
'!gxeps -c -R -i HIS -o woce_p13_t.eps'
*----------------------------------------


*--------------- Salinity  -------------------
'reinit'
'open hs_s.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=ss'
rc = wocep13(aa,0.1,0.05,2,2,34,35.7)
'!gxeps -c -R -i HIS -o woce_p13_s.eps'
*----------------------------------------

*--------------- DIC (anthropogenic)-------------------
'reinit'
'open hstcbnNPZD.pctl'
'open hstcbnNPZD_ref.pctl'
'open hs_sigma0.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=(dic-dic.2)*1000*1000/(1000+sigma0.3)+0.01'
rc = wocep13(aa,5,5,2,2,0,60)
'!gxeps -c -R -i HIS -o woce_p13_dic_a.eps'
*----------------------------------------

*--------------- DIC -------------------
'reinit'
'open hstcbnNPZD.pctl'
'open hstcbnNPZD_ref.pctl'
'open hs_sigma0.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=dic*1000*1000/(1000+sigma0.3)'
rc = wocep13(aa,10,20,5,2,1900,2400)
'!gxeps -c -R -i HIS -o woce_p13_dic.eps'
*----------------------------------------


*--------------- Alkalinity -------------------
'reinit'
'open hstcbnNPZD.pctl'
'open hstcbnNPZD_ref.pctl'
'open hs_sigma0.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=alk*1000*1000/(1000+sigma0.3)'
rc = wocep13(aa,10,20,5,2,2300,2500)
'!gxeps -c -R -i HIS -o woce_p13_alk.eps'
*----------------------------------------


*--------------- CFC -------------------
'reinit'
'open hstcbnNPZD.pctl'
'open hstcbnNPZD_ref.pctl'
'open hs_sigma0.pctl'
'open hsCFC.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=cfc11.4*1e9*1000/(1000+sigma0.3)'
rc = wocep13(aa,0.4,0.02,1,2,0,6)
'!gxeps -c -R -i HIS -o woce_p13_cfc11.eps'
*----------------------------------------

*--------------- Oxygen -------------------
'reinit'
'open hstcbnNPZD.pctl'
'open hstcbnNPZD_ref.pctl'
'open hs_sigma0.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=o2*1000*1000/(1000+sigma0.3)'
rc = wocep13(aa,10,20,5,2,20,280)
'!gxeps -c -R -i HIS -o woce_p13_o2.eps'
*----------------------------------------


*--------------- Nitrate -------------------
'reinit'
'open hstcbnNPZD.pctl'
'open hstcbnNPZD_ref.pctl'
'open hs_sigma0.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
'set z 1 50'
'define aa=no3*1000*1000/(1000+sigma0.3)'
rc = wocep13(aa,5,1,2,2,5,45)
'!gxeps -c -R -i HIS -o woce_p13_no3.eps'
*----------------------------------------

function wocep13(var1,cint1,cint2,cl1,cl2,rb1,rb2)
*----------------------------------------------------------
*'open hstcbnNPZD.pctl'
*'open hstcbnNPZD_ref.pctl'
*'open hs_sigma0.pctl'
'set lon 165'
'set lat -4 60'
'set time 1aug1992'
*'set vpage 0 11 5 8'
*'set parea 0.5 10.5 6 8' 
'set parea 0.5 9.5 6 8' 
'set mproj scaled'
'set yflip on'
'set grads off'
'set lev 0 1000'
'set ylint 200'
'set cint 'cint1
'set gxout shaded'
'set rbrange 'rb1' 'rb2
'd 'var1
'set gxout contour'
'set cint 'cint1
'set clskip 'cl1
'd 'var1

*'set vpage 0 11 0 6'
'set mproj scaled'
*'set parea 0.5 10.5 1 5.5' 
'set parea 0.5 9.5 1 5.5' 
'set yflip on'
'set grads off'
'set lev 0 6500'
'set ylint 500'
'set cint 'cint2
'set rbrange 'rb1' 'rb2
'set gxout shaded'
'd 'var1
'set gxout contour'
'set cint 'cint2
'set ccolor 1'
'set rbrange 'rb1' 'rb2
'set clskip 'cl2
'd 'var1
'run cbarn'

'enable print HIS'
'print'
'disable print HIS'
return




