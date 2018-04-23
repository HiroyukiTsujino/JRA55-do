#!/bin/bash

set -e

driver=fft_pgi_driver
make ${driver}
./${driver}

exit 0
