dset ^/work113/htsujino/GlobCurrent/global_025_deg/total_hs/%y4/%y4%m2%d2-GLOBCURRENT-L4-CUReul_hs-ALT_SUM-v03.0-fv01.0.nc
title AVISO SSH products (0.25x0.25)
options  template
DTYPE netcdf
undef -2147483647 _FillValue
unpack scale_factor 
xdef 1440 LINEAR -179.875  0.25
ydef 720 linear   -89.875  0.25
zdef 1 levels 0.000000
tdef 10000 linear 1Jan1993 1dy
vars 2
eastward_eulerian_current_velocity=>u    1   t,y,x  total velocity : zonal component (m/s)
northward_eulerian_current_velocity=>v   1   t,y,x  total velocity : meridional component (m/s)
endvars
