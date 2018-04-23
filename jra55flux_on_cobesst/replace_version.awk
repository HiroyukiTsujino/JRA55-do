#!/usr/bin/gawk -f
{
    if (gsub("v1_1","v0_1",$0))
      print
    else
      print
    endif
}
