# -*- coding: utf-8 *-

import os, sys
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
from datetime import datetime, timedelta

if (len(sys.argv) < 1):
    print ("Usage: " + sys.argv[0] + "Pacific (E) or Atlantic (L) ")
    sys.exit()

basin=sys.argv[1]

nn=0

year=[]
TC_name=[]
yyyy=[]
mm=[]
dd=[]
hh=[]
lat=[]
lon=[]

for line in open ("data/anti-cyclonic_TCs_"+ basin + "_en_linux.txt","r"):

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

start_lat=[]
start_lon=[]

for nn in range(0,valid_analy):

    print ("")
    print (nn, year[nn], TC_name[nn])
    print (yyyy[nn], mm[nn], dd[nn], hh[nn], lat[nn], lon[nn], cont[nn])
    analy_time = datetime(year=yyyy[nn], month=mm[nn], day=dd[nn], hour=hh[nn])

    if (nn == 0 or (nn > 0 and TC_name[nn] != TC_name[nn-1])):
        if (ntc == 0):
            start_draw.insert(0,analy_time)
            start_anl.insert(0,nn)
            start_lon.insert(0,lon[nn])
            start_lat.insert(0,lat[nn])

        else:
            start_draw.append(analy_time)
            start_anl.append(nn)
            start_lon.append(lon[nn])
            start_lat.append(lat[nn])

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
#
#nx=640
#ny=320
#
#lonW = 0.0
#lonE = 359.4375
#longitude = np.linspace(lonW,lonE,nx)
#print(longitude)
#
#latitude = np.loadtxt("../linkdir/data/lat_jra55_exact.txt")
#print(latitude)
#
#slprs_v1_3 = np.array(np.empty((ny,nx)),dtype=np.float32)
#slprs_v1_5 = np.array(np.empty((ny,nx)),dtype=np.float32)
#slprs_20CR = np.array(np.empty((ny,nx)),dtype=np.float32)
#
#-------------------------

num_tc = ntc
for ntc in range(0,num_tc):
   print("")
   print("TC No.",ntc)
   print("Center at",start_lon[ntc],start_lat[ntc])
   print("Drawing from",start_draw[ntc],"to",end_draw[ntc])

   nana = start_anl[ntc]

   fln_shell='scripts/'+'TC'+start_draw[ntc].strftime('%Y')+'-'+TC_name[start_anl[ntc]]+'.sh'
   print(fln_shell)
   fo=open(fln_shell,mode="w")
   fo.write("#!/bin/bash\n")
   fo.write("cat <<EOF > hoge\n")

   #print(start_draw[ntc].strftime('%Y%m%d%H'),end_draw[ntc].strftime('%Y%m%d%H'))
   #print(start_anl[ntc], end_anl[ntc])

   draw_first = start_draw[ntc]

   nfig = 1
   nc = 0
   nd = 0
   while (draw_first <= end_draw[ntc]):

       if (nana < end_anl[ntc]):
           nfirst = nana
       else:
           nfirst = end_anl[ntc]

       draw_current = draw_first
       gradstime = draw_current.strftime('%Hz%d%b%Y')
       print(gradstime,'TC'+start_draw[ntc].strftime('%Y')+'-'+'{:02}'.format(ntc)+'-'+'{:02}'.format(nfig))
       grads_command='\"run gscript/slprs_vector_comp.specify.gs ' + start_draw[ntc].strftime('%Y') \
                 + ' ' + TC_name[start_anl[ntc]]+ '-' + '{:02}'.format(nfig)                        \
                 + ' ' + gradstime +  ' ' + str(lon[nfirst]) + ' ' + str(lat[nfirst])+'\"\n'
       fo.write(grads_command)

       for nt in range(4):
           nd += 1
           #yyyymm = draw_current.strftime('%Y%m')
           #yyyymmddhh = draw_current.strftime('%Y%m%d%H')
           if (nc == 0):
               nc += 1
           else:
               nana = nana + 1
               nc = 0

           draw_current = draw_current + timedelta(0,10800)

       draw_first = draw_first + timedelta(0,43200)
       nfig+=1

   fo.write("\"quit\"\n")
   fo.write("EOF\n")
   fo.write("grads -bpc hoge\n")
   fo.write("rm -f hoge\n")
   fo.close()
