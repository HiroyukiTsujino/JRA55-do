#!/bin/sh

dofm() {
    case $1 in
	1 ) echo 31 ;;
	2 ) echo 28 ;;
	3 ) echo 31 ;;
	4 ) echo 30 ;;
	5 ) echo 31 ;;
	6 ) echo 30 ;;
	7 ) echo 31 ;;
	8 ) echo 31 ;;
	9 ) echo 30 ;;
	10 ) echo 31 ;;
	11 ) echo 30 ;;
	12 ) echo 31 ;;
	* ) echo 0 ;;
    esac
}

ndsmon() {
    mon=$1
    nd=0
    m=1
    while [ $m -lt $mon ]; do
	ndd=`dofm $m`
	nd=`expr $nd + $ndd`
        m=`expr $m + 1`
    done
    echo $nd
}

isleap() {
    iyear=$1
    n=0
    m4=`expr $iyear % 4` || :
    m100=`expr $iyear % 100` || :
    m400=`expr $iyear % 400` || :
    if [ $m4 -eq 0 ]; then
	n=1
    fi
    if [ $m100 -eq 0 ]; then
	n=0
    fi
    if [ $m400 -eq 0 ]; then
	n=1
    fi
    echo $n
}

ndatey() {
# 年初からの日数
    iyear=$1
    mon=$2
    iday=$3
    nd=`ndsmon $mon`
    nd=`expr $nd + $iday`
    if [ $mon -ge 3 ]; then
	nleap=`isleap $iyear`
	nd=`expr $nd + $nleap`
    fi
    echo $nd
}

nydate() {
# 年初から NDAY 日目の日付
    iyear=$1
    nday=$2
#
    nn=$nday
# 1月
    ndm=`dofm 1`
    nn=`expr $nn - $ndm` || :
    if [ $nn -le 0 ]; then
	mon=1
	iday=`expr $nn + $ndm`
    fi
# 2月
    if [ $nn -gt 0 ]; then
	ndm=`dofm 2`
	nleap=`isleap $iyear`
	nn=`expr $nn - $ndm - $nleap` || :
	if [ $nn -le 0 ]; then
	    mon=2
	    iday=`expr $nn + $ndm + $nleap`
	fi
    fi
# 3月以降
    m=3
    while [ $nn -gt 0 -a $m -le 12 ]; do
	ndm=`dofm $m`
	nn=`expr $nn - $ndm` || :
	if [ $nn -le 0 ]; then
	    mon=$m
	    iday=`expr $nn + $ndm`
	    break # exit from while loop
	fi
	m=`expr $m + 1`
    done
    echo $iyear $mon $iday
}
    
nddate() {
# 与えられた日付から NDAY 日後の日付
    iyear=$1
    mon=$2
    iday=$3
    nday=$4
    nd=`ndatey $iyear $mon $iday`
    nd=`expr $nday + $nd`
    while [ $nd -gt 0 ]; do
	nleap=`isleap $iyear`
	nd=`expr $nd - 365 - $nleap` || :
	iyear=`expr $iyear + 1`
    done
    iyear=`expr $iyear - 1`
    nleap=`isleap $iyear`
    nd=`expr $nd + 365 + $nleap`
    nydate $iyear $nd
}

nddateb() {
# 与えられた日付から NDAY 日前の日付
    iyear=$1
    mon=$2
    iday=$3
    nday=$4
    nd=`ndatey $iyear $mon $iday`
    nd=`expr $nday - $nd` || :
    while [ $nd -ge 0 ]; do
	iyear=`expr $iyear - 1`
	nleap=`isleap $iyear`
	nd=`expr $nd - 365 - $nleap` || :
    done
    nleap=`isleap $iyear`
    nd=`expr $nd + 365 + $nleap`
    nd=`expr 365 + $nleap - $nd`
    nydate $iyear $nd
}
