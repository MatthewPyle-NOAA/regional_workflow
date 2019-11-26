#!/bin/sh
set -eux
#------------------------------------
# USER DEFINED STUFF:
#
# USE_PREINST_LIBS: set to "true" to use preinstalled libraries.
#                   Anything other than "true"  will use libraries locally.
#------------------------------------

export USE_PREINST_LIBS="true"

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------

build_dir=`pwd`
logs_dir=$build_dir/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

#------------------------------------
# build libraries first
#------------------------------------
$Build_libs && {
echo " .... Library build not currently supported .... "
#echo " .... Building libraries .... "
#./build_libs.sh > $logs_dir/build_libs.log 2>&1
}

#------------------------------------
# build forecast
#------------------------------------
$Build_forecast && {
echo " .... Building forecast .... "
./build_forecast.sh > $logs_dir/build_forecast.log 2>&1
}

#------------------------------------
# build forecast ccpp
#------------------------------------
$Build_forecast_ccpp && {
echo " .... Building forecast ccpp.... "
export CCPP=true
./build_forecast.sh > $logs_dir/build_forecast_ccpp.log 2>&1
}

#------------------------------------
# build post
#------------------------------------
$Build_post && {
echo " .... Building post .... "
./build_post.sh > $logs_dir/build_post.log 2>&1
}

#------------------------------------
# build bufr
#------------------------------------
$Build_bufr && {
echo " .... Building bufr .... "
./build_bufr.sh > $logs_dir/build_bufr.log 2>&1
}

#------------------------------------
# build sndp
#------------------------------------
$Build_sndp && {
echo " .... Building sndp .... "
./build_sndp.sh > $logs_dir/build_sndp.log 2>&1
}

#------------------------------------
# build fv3snowbucket
#------------------------------------
$Build_fv3snowbucket && {
echo " .... Building fv3snowbucket .... "
./build_fv3snowbucket.sh > $logs_dir/build_fv3snowbucket.log 2>&1
}

#------------------------------------
# build bucket
#------------------------------------
$Build_bucket && {
echo " .... Building bucket .... "
./build_bucket.sh > $logs_dir/build_bucket.log 2>&1
}



#------------------------------------
# build utils
#------------------------------------
$Build_utils && {
echo " .... Building utils .... "
./build_utils.sh > $logs_dir/build_utils.log 2>&1
}


echo;echo " .... Build system finished .... "

exit 0
