#! /bin/sh

# FV3 build

./manage_externals/checkout_externals

cd fv3

./link_fix.sh

/gpfs/dell1/nco/ops/nwtest/upgrade_utils.v0.0.2/exec/checkoutsidecompilefiles ./build_all.sh >& build_all.log

exit

# ARW build

cd ../arw

./link_fix.sh

./build_hiresw.sh
