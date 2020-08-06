# -*- coding: utf-8 *-

import os, sys
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
from datetime import datetime, timedelta

if (len(sys.argv) < 4) :
    print ("Usage: " + sys.argv[0] + "item(jra55) item(20CR) item(output) ")
    sys.exit()

item_v1_3=sys.argv[1]
item_20CR=sys.argv[2]
item_v1_5=sys.argv[3]

path_slprs_v1_3="../linkdir/forcing/jra55fcst_filt_3hr_TL319"
path_slprs_v1_5="../linkdir/forcing/jra55fcst_v1_5_3hr_TL319"
path_slprs_20CR="../linkdir/forcing/20CRv3_3hr_TL319"

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

ntc = 0
start_draw=[]
end_draw=[]
start_anl=[]
end_anl=[]

for nn in range(0,valid_analy):

    print ("")
    print (nn, year[nn], TC_name[nn])
    print (yyyy[nn], mm[nn], dd[nn], hh[nn], lat[nn], lon[nn], cont[nn])
    analy_time = datetime(year=yyyy[nn], month=mm[nn], day=dd[nn], hour=hh[nn])

    if (nn == 0 or (nn > 0 and TC_name[nn] != TC_name[nn-1])):
        if (ntc == 0):
            start_draw.insert(0,analy_time)
            start_anl.insert(0,nn)

        else:
            start_draw.append(analy_time)
            start_anl.append(nn)

    if (nn < valid_analy-1 and TC_name[nn] == TC_name[nn+1]):
        cont[nn] = 1

    if (cont[nn] == 0):
        if (ntc == 0):
            end_draw.insert(0,analy_time + timedelta(3,21600))
            end_anl.insert(0,nn)
        else:
            end_draw.append(analy_time + timedelta(3,21600))
            end_anl.append(nn)

        print("TC No.",ntc,start_draw[ntc],end_draw[ntc])

        ntc += 1

#-------------------------

nx=640
ny=320

lonW = 0.0
lonE = 359.4375
longitude = np.linspace(lonW,lonE,nx)
print(longitude)

latitude = np.loadtxt("../linkdir/data/lat_jra55_exact.txt")
print(latitude)

slprs_v1_3 = np.array(np.empty((ny,nx)),dtype=np.float32)
slprs_v1_5 = np.array(np.empty((ny,nx)),dtype=np.float32)
slprs_20CR = np.array(np.empty((ny,nx)),dtype=np.float32)

#-------------------------

num_tc = ntc
#for ntc in range(0,num_tc):
for ntc in range(1):
   print("TC No.",ntc)
   #print(start_draw[ntc].strftime('%Y%m%d%H'),end_draw[ntc].strftime('%Y%m%d%H'))
   #print(start_anl[ntc], end_anl[ntc])
   draw_first = start_draw[ntc]

   while (draw_first <= end_draw[ntc]):
       #fig = plt.figure(figsize=(8,11))
       ##fig.suptitle( suptitle, fontsize=18 )
       #proj = ccrs.PlateCarree(central_longitude=-140.)
       #lon_formatter = LongitudeFormatter(zero_direction_label=True)
       #lat_formatter = LatitudeFormatter()
       #ax = [
       #    plt.subplot(3,4,1,projection=proj),
       #    plt.subplot(3,4,2,projection=proj),
       #    plt.subplot(3,4,3,projection=proj),
       #    plt.subplot(3,4,4,projection=proj),
       #    plt.subplot(3,4,5,projection=proj),
       #    plt.subplot(3,4,6,projection=proj),
       #    plt.subplot(3,4,7,projection=proj),
       #    plt.subplot(3,4,8,projection=proj),
       #    plt.subplot(3,4,9,projection=proj),
       #    plt.subplot(3,4,10,projection=proj),
       #    plt.subplot(3,4,11,projection=proj),
       #    plt.subplot(3,4,12,projection=proj),
       #    ]

       draw_current = draw_first
       for nt in range(4):
           yyyymm = draw_current.strftime('%Y%m')
           yyyymmddhh = draw_current.strftime('%Y%m%d%H')
           file_slprs_v1_3 = path_slprs_v1_3 + '/' + str(yyyymm) + '/' + item_v1_3 + '.' + str(yyyymmddhh)
           file_slprs_v1_5 = path_slprs_v1_5 + '/' + str(yyyymm) + '/' + item_v1_5 + '.' + str(yyyymmddhh)
           file_slprs_20CR = path_slprs_20CR + '/' + str(yyyymm) + '/' + item_20CR + '.' + str(yyyymmddhh)
           print()
           print(file_slprs_v1_5)
           print(file_slprs_20CR)
           print('Reading from '+ file_slprs_v1_3)
           f1 = open(file_slprs_v1_3,'rb')
           f2 = open(file_slprs_v1_5,'rb')
           f3 = open(file_slprs_20CR,'rb')
           for j in range(ny):
               slprs_v1_3[j,:] = np.fromfile(f1, dtype = '>f', count = nx)
               slprs_v1_5[j,:] = np.fromfile(f2, dtype = '>f', count = nx)
               slprs_20CR[j,:] = np.fromfile(f3, dtype = '>f', count = nx)

           f3.close()
           f2.close()
           f1.close()

           draw_current = draw_current + timedelta(0,10800)

       draw_first = draw_first + timedelta(0,43200)

   print(start_draw[ntc],end_draw[ntc])


#tcn=0
#for nn in range(0,valid_analy):
#
#    namelist_in="namelist/namelist.merge20CRv3jra55_"+str(year[nn])+"_"+str(TC_name[nn])+"_"+str(tcn).zfill(2)
#
#    f1 = open("namelist.merge20CRv3jra55_template","r")
#    f2 = open(namelist_in,"w")
#    for row in f1:
#        temp = row.replace("@year@",str(yyyy[nn])) \
#                  .replace("@mon@",str(mm[nn])) \
#                  .replace("@day@",str(dd[nn])) \
#                  .replace("@hour@",str(hh[nn])) \
#                  .replace("@cont@",str(cont[nn])) \
#                  .replace("@lon@",str(lon[nn])) \
#                  .replace("@lat@",str(lat[nn])) \
#                  .replace("@item_jra55@",str(item_jra55)) \
#                  .replace("@item_20CR@",str(item_20CR)) \
#                  .replace("@item_out@", str(item_out))
#        f2.write(temp)
#    
#    if (nn < valid_analy-1 and TC_name[nn] == TC_name[nn+1]):
#        tcn+=1
#    else:
#        tcn=0
#
#    f2.close
#    f1.close
#
