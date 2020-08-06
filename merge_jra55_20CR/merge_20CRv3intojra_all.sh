#!/bin/bash

set -e

source activate base

# note overwrite is not allowed
# please do in one operation

# Input argument for python: item(jra55) item(20CR) item(output)
#python ./extract_tcs.py u10m uwnd.10m u10m  #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py v10m vwnd.10m v10m  #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py slprs prmsl slprs   #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py tmp10m tmp10m tmp10m  #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py sph10m sph10m sph10m  #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py dswrf dswrf.sfc dswrf #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py dlwrf dlwrf.sfc dlwrf #done on 25Feb2020(E), 25Feb2020(L)
#python ./extract_tcs.py prcp prate prcp       #done on 25Feb2020(E), 25Feb2020(L)

for i in `ls -1 ./namelist`
do
  ln -sfn namelist/${i} namelist.merge20CRv3jra55  
  ./merge_20CRv3_and_jra
done
