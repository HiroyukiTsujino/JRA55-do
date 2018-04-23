#!/bin/csh -f

@ yrst = 2016
@ yred = 2017

set orgdir=/home/tsuzuki/DATA_pub/JRA-55/fcst_phyland/
set newdir=/home/tsuzuki/JRA55_RIV/DATA/T319bin1dy

mkdir -p $newdir

################

@ year = ${yrst}

while ( ${year} <= ${yred} )


#### set leapyear ###############################
@ iy1 = $year - ( $year / 400 )  * 400 
@ iy2 = $year - ( $year / 100 )  * 100 
@ iy3 = $year - ( $year / 4 ) * 4
if( $iy1 == 0 ) then
set imon = ( 31 29 31 30 31 30 31 31 30 31 30 31 )
@ leap = 1
else if( $iy2 == 0 ) then
set imon = ( 31 28 31 30 31 30 31 31 30 31 30 31 )
@ leap = 0
else if( $iy3 == 0 ) then
set imon = ( 31 29 31 30 31 30 31 31 30 31 30 31 )
@ leap = 1
else
set imon = ( 31 28 31 30 31 30 31 31 30 31 30 31 )
@ leap = 0
endif
##################################################


@ month = 1
#foreach dyed ( $imon )
while( $month <= 12 )
@ dyed = $imon[$month]

set orgdir=/home/tsuzuki/DATA_pub/JRA-55/fcst_phyland/`printf %04d $year``printf %02d $month`

@ day = 1
while ( $day <= $dyed )

set newfile=${newdir}/watr`printf %04d $year``printf %02d $month``printf %02d $day`
echo -n > $newfile

foreach hour ( 0 3 6 9 12 15 18 21 )

set orgfile=${orgdir}/fcst_phyland.`printf %04d $year``printf %02d $month``printf %02d $day``printf %02d $hour`
echo $orgfile

wgrib -s $orgfile | egrep '(:WATR:)' | wgrib -i -bin -ieee $orgfile  -append -o $newfile

end


@ day = $day + 1
end
@ month = $month + 1
end
@ year = $year + 1
end



