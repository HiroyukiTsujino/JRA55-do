#!/bin/bash

for sta in ALE BAR GVN NSA NYA SYO TIK
do
  sed -e s%@sta@%${sta}% namelist.station_dswrf_template > namelist.station
  ./txt2grads_station
done
