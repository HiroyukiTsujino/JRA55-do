#!/bin/bash

logs=`ls log.*`

for logfile in ${logs}; do

  flag_fail=false

  if [ "`grep SUCCESSFUL ${logfile}`" = "" ]; then
    flag_fail=true
    flag_fail_all=true
  fi

  assert=`cat ${logfile} | tail -n 3 | head -n 1`
  assert=${assert#*:}

  if [ ${flag_fail} = "true" ]; then
    echo ${logfile} ${assert} "!!!! FAIL !!!!"
    exit 1
  else
    echo ${logfile} ${assert}
  fi

done

exit 0
