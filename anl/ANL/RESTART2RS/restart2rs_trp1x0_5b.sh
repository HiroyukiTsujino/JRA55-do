#! /bin/sh
#
#

flin="$1"
ext_year=$2
ext_month=$3
floutpath="$4"

imut=364
jmut=368
km=51

ext_day=1
ext_hour=0
ext_minute=0
ext_second=0

l_oflt=.false.
l_vvdimp=.true.
l_melyam=.false.
l_nohkim=.true.

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

