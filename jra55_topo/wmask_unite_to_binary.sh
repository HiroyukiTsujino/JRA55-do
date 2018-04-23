#!/bin/bash

exe=mk_wind_fill_unite
cgrid=U

#indxdir=tropics
#file_mask=tropics_mask

#indxdir=so
#file_mask=so_mask

#indxdir=np
#file_mask=np_mask

#indxdir=tio
#file_mask=tio_mask

#indxdir=pac
#file_mask=pac_mask

indxdir=wind_fill
file_mask=jra55_wind_fill

./${exe}<<EOF
&nml_maskunite
  cgrid="${cgrid}",
  cdir="${indxdir}"
  flout_base="${file_mask}"
/
EOF

exit 0
