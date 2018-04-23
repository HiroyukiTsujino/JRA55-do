#! /bin/sh
#
#

flin="$1"
ext_year=$2
ext_month=$3
ext_day=$4
ext_hour=$5
floutpath="$6"

imut=184
jmut=152
km=51

ext_minute=0
ext_second=0

l_oflt=.false.
l_vvdimp=.true.
l_melyam=.false.
l_nohkim=.false.

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./restart2rs<<EOF
 &nml_restart2rs
 flin="$flin"
 floutpath="$floutpath"
 imut=$imut
 jmut=$jmut
 km=$km
 ext_year=$ext_year
 ext_month=$ext_month
 ext_day=$ext_day
 ext_hour=$ext_hour
 ext_minute=$ext_minute
 ext_second=$ext_second
 l_oflt=$l_oflt
 l_vvdimp=$l_vvdimp
 l_melyam=$l_melyam
 l_nohkim=$l_nohkim
 /
EOF

