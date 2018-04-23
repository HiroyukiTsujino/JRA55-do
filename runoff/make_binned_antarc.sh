#!/bin/bash

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dsname"
  exit
fi

if [ -e namelist.runoff_integ_antarc ]; then
  rm -f namelist.runoff_integ_antarc
fi
ln -sf namelist.runoff_integ_antarc_${1} namelist.runoff_integ_antarc

./runoff_antarctica_region
