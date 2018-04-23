#!/usr/bin/gawk -f
{
    if (gsub("\\^\\..\\/wgomd/ARCTIC/","^",$0))
      print
    else
      print
    endif
}
