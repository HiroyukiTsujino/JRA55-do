function getdate()


'q dims'
*say result
date=sublin(result,5)
date=subwrd(date,6)

if (substr(date,3,1)='Z')
* minute is omitted
  year=substr(date,9,4)
  month=substr(date,6,3)
  day=substr(date,4,2)
  hour=substr(date,1,2)
  hour=hour':00'
else
  year=substr(date,12,4)
  month=substr(date,9,3)
  day=substr(date,7,2)
  hour=substr(date,1,5)
endif

if (month = 'JAN')
  month = '01'
endif
if (month = 'JAN')
  month = '01'
endif
if (month = 'FEB')
  month = '02'
endif
if (month = 'MAR')
  month = '03'
endif
if (month = 'APR')
  month = '04'
endif
if (month = 'MAY')
  month = '05'
endif
if (month = 'JUN')
  month = '06'
endif
if (month = 'JUL')
  month = '07'
endif
if (month = 'AUG')
  month = '08'
endif
if (month = 'SEP')
  month = '09'
endif
if (month = 'OCT')
  month = '10'
endif
if (month = 'NOV')
  month = '11'
endif
if (month = 'DEC')
  month = '12'
endif

date=year%'/'%month%'/'%day%'-'%hour

return date

