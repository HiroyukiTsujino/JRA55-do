# -*- coding: utf-8 *-

import os, sys
import subprocess

if (len(sys.argv) < 4) :
    print ("Usage: " + sys.argv[0] + "item(jra55) item(20CR) item(output) ")
    sys.exit()

item_jra55=sys.argv[1]
item_20CR=sys.argv[2]
item_out=sys.argv[3]

nn=0

year=[]
TC_name=[]
yyyy=[]
mm=[]
dd=[]
hh=[]
lat=[]
lon=[]

#for line in open ("data/anti-cyclonic_TCs_E_en_linux.txt","r"):
for line in open ("data/anti-cyclonic_TCs_L_en_linux.txt","r"):

    if line[0] == "":
        continue

    data = line.split()

    if len(data) < 4:
        continue

    if (data[0][0:4] == "Year"):
        continue

    if (nn == 0):
        year.insert(0,int(data[0][0:4]))
        TC_name.insert(0,data[0][5:])
        yyyy.insert(0,int(data[1][0:4]))
        mm.insert(0,int(data[1][4:6]))
        dd.insert(0,int(data[1][6:8]))
        hh.insert(0,int(data[1][8:10]))
        lat.insert(0,float(data[2]))
        lon.insert(0,float(data[3]))
    else:
        year.append(int(data[0][0:4]))
        TC_name.append(data[0][5:])
        yyyy.append(int(data[1][0:4]))
        mm.append(int(data[1][4:6]))
        dd.append(int(data[1][6:8]))
        hh.append(int(data[1][8:10]))
        lat.append(float(data[2]))
        lon.append(float(data[3]))

    nn += 1

valid_analy = nn
cont=[0]*valid_analy

for nn in range(0,valid_analy):

    if (nn < valid_analy-1 and TC_name[nn] == TC_name[nn+1]):
        cont[nn] = 1

    print (nn, year[nn], TC_name[nn])
    print (yyyy[nn], mm[nn], dd[nn], hh[nn], lat[nn], lon[nn], cont[nn])

tcn=0
for nn in range(0,valid_analy):

    namelist_in="namelist/namelist.merge20CRv3jra55_"+str(year[nn])+"_"+str(TC_name[nn])+"_"+str(tcn).zfill(2)

    f1 = open("namelist.merge20CRv3jra55_template","r")
    f2 = open(namelist_in,"w")
    for row in f1:
        temp = row.replace("@year@",str(yyyy[nn])) \
                  .replace("@mon@",str(mm[nn])) \
                  .replace("@day@",str(dd[nn])) \
                  .replace("@hour@",str(hh[nn])) \
                  .replace("@cont@",str(cont[nn])) \
                  .replace("@lon@",str(lon[nn])) \
                  .replace("@lat@",str(lat[nn])) \
                  .replace("@item_jra55@",str(item_jra55)) \
                  .replace("@item_20CR@",str(item_20CR)) \
                  .replace("@item_out@", str(item_out))
        f2.write(temp)
    
    if (nn < valid_analy-1 and TC_name[nn] == TC_name[nn+1]):
        tcn+=1
    else:
        tcn=0

    #link_command=["ln", "-sfn", namelist_in, "namelist.merge20CRv3jra55"]
    #subprocess.call(link_command)
    #subprocess.call("./merge_20CRv3_and_jra")

    f2.close
    f1.close
