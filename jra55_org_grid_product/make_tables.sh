#!/bin/bash -f

set -e

ln -sf namelist.make_table_fill_water namelist.make_table
./make_mapping_table

ln -sf namelist.make_table_linear namelist.make_table
./make_mapping_table

\rm namelist.make_table
