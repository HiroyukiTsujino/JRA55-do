#! /bin/sh
#
set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dsversion"
fi

ver=${1}

ln -sf namelist.sverdrup_function_${ver} namelist.sverdrup_function

./sverdrup
