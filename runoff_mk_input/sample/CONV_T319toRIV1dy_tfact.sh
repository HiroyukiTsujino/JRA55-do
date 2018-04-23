#!/bin/csh -f

cd /home/tsuzuki/JRA55_RIV

@ yrst = 2017
@ yred = 2017

set datdir=/home/tsuzuki/JRA55_RIV/DATA/

################

set mask=${datdir}/TL319.mask

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
while ( $month <= 12 )
@ dyed = $imon[$month]

@ day = 1
while ( $day <= $dyed )

set infile=${datdir}/T319bin1dy/watr`printf %04d $year``printf %02d $month``printf %02d $day`
#set oufile=${datdir}/RIVGRID_1dy/rof`printf %04d $year``printf %02d $month``printf %02d $day`.grd
set oufile=${datdir}/RIVGRID_1dy_tfact2/rof`printf %04d $year``printf %02d $month``printf %02d $day`.grd

if( $year <= 2015 ) then
@ yearm = $year
else
@ yearm = 2015
endif

set fact=${datdir}/TFACT2_rivmap160805/tfact`printf %04d $yearm``printf %02d $month``printf %02d $day`.grd

echo $oufile

rm namelist
echo " &nmlst ifile='${infile}',ofile='${oufile}',mfile='${mask}',fact=t, factor='${fact}' &end " > namelist
./CONV_T319toRIV1dy_tfact/src/conv_riv < ./namelist

@ day = $day + 1
end
@ month = $month + 1
end
@ year = $year + 1
end



