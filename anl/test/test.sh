#!/bin/bash

. test/dir_list.sh

dir_now=`pwd`

for dir in ${dir_list}; do
  echo
  echo ${dir}:
  cd ${dir}
  sh test/test_all.sh || exit 1
  make clean > /dev/null 2> /dev/null
  cd ${dir_now}
done


exit 0
