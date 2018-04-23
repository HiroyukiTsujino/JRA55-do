#!/bin/bash

target_file=${1}

echo ${target_file}

#corename=`eval basename ${target_file} "ctl"`

replace_version.awk ${target_file} > tmp.ctl
#replace_version.awk ${target_file} > tmp.ctl

mv tmp.ctl ${target_file}
