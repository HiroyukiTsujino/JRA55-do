#!/usr/bin/awk
#
# usage: gawk -f extract_lat.awk input_file

BEGIN{
FS = " "
}
{
for ( i=1; i <=NF; i++ )
print $i
}
