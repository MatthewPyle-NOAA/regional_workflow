#! /bin/sh

#################################
# FV3 build
#################################

# 1st time only
./manage_externals/checkout_externals

cd fv3

# 1st time only
./link_fix.sh

# /gpfs/dell1/nco/ops/nwtest/upgrade_utils.v0.0.2/exec/checkoutsidecompilefiles ./build_all.sh >& build_all_fv3.log
./build_all.sh >& build_all_fv3.log

exit

#################################
# ARW build
#################################

cd ../arw

#1st time only
./link_fix.sh

# /gpfs/dell1/nco/ops/nwtest/upgrade_utils.v0.0.2/exec/checkoutsidecompilefiles ./build_hiresw.sh >& build_all_arw.log
./build_hiresw.sh >& build_all_arw.log
