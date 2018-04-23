**************************************************************
* save Ver 0.02
*
* function:
*   save image as gmf/eps format
* usage:
*   save file-head
*     file-head : filename before . 
*       (ex. file-head=test -> save as test.eps)
* note:
*   This function uses gxeps command.
**************************************************************
function save(args)


*** arguements ***
fhead=subwrd(args,1)


*** save ***
'enable print 'fhead'.gmf'
'print'
'disable'
'!gxeps -c -i 'fhead'.gmf -o 'fhead'.eps'
*'!gxps -c -i 'fhead'.gmf -o 'fhead'.ps'
'!rm -f 'fhead'.gmf'
return
