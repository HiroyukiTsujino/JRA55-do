#! /bin/sh
#
# Usage: stddev.sh fseriescore snum fincore flout tuxy dnum sttyear endyear lag

fincore="$1"
flout="$2"
undef=$3
inum=$4
jnum=$5
knum=$6
vnum=$7
sttyear=$8
endyear=$9
l_monthly=${10}

########################################

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./stddev<<EOF
&nml_stddev
fincore="$fincore"
flout="$flout"
undef=$undef
l_monthly=$l_monthly
inum=$inum
jnum=$jnum
knum=$knum
vnum=$vnum
sttyear=$sttyear
endyear=$endyear
/
EOF


