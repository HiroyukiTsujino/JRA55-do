#!/bin/bash
#


if [ -f setting/make_macros.make.sh ]; then
  (cd setting; sh make_macros.make.sh )
  if [ $? -eq 0 ]; then
    echo "macros.make is created in setting/."
  else
    echo "Making macros.make in setting/ failed."
  fi
else
  echo "The file \"make_macros.make.sh\" is not found in setting/. Please create it." 
  echo "There are sample files in the setting/sample/."
fi


echo
if [ -f linkdir/link.sh ]; then
  (cd linkdir; sh link.sh )
  if [ $? -eq 0 ]; then
    echo "links are created in linkdir"
  else
    echo "Creating links in linkdir failed."
  fi
else
  echo "The file \"link.sh\" is not found in linkdir. Please create it." 
  echo "There are sample files in the linkdir/sample/."
fi

exit 0
