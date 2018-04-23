#!/bin/bash


driver=density_driver

make ${driver} || exit 1

./${driver} || exit 1

exit 0
