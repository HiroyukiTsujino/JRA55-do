#!/bin/bash -f

orgdir=../linkdir/forcing/const
newdir=../linkdir/forcing/jra_org/const

#wgrib -s ${orgdir}/TL319.grib | egrep '(:LAND:)' | wgrib -i -bin -ieee -nh ${orgdir}/TL319.grib -o ${newdir}/TL319.mask
wgrib -s ${orgdir}/TL319.grib | egrep '(:LAND:)' | wgrib -i -bin -nh ${orgdir}/TL319.grib -o ${newdir}/TL319.mask
