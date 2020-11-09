#! /bin/sh
set -eux
module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load ../modulefiles_arw/HIRESW/v8.0.0
module list


##################################

# setting a BUILD* variable to 1 means
# that build script will run.
# 
# setting a BUILD* variable to 0 (technically anything but 1)
# will skip the build of that component.

BUILD_hiresw_wrfarwfcst=1
BUILD_hiresw_bucket=1
BUILD_hiresw_wrfbufr=1
BUILD_hiresw_wps=1
BUILD_hiresw_post=1
BUILD_hiresw_smartinitg2=1
BUILD_hiresw_smartprecipg2=1
BUILD_hiresw_sndp=1
BUILD_hiresw_stnmlist=1

##################################
#
# SHOULD REQUIRE NO CHANGES BELOW
#
##################################

export BASE=`pwd`

if [ ! -e $BASE/logs ]
then
mkdir $BASE/logs
fi

export logs_dir=$BASE/logs

set +x

if [ $BUILD_hiresw_wrfarwfcst -eq 1 ]
then

echo "executing build_hiresw_wrfarwfcst"
./build_hiresw_wrfarwfcst.sh >& ${logs_dir}/build_hiresw_wrfarwfcst.log
echo hiresw_wrfarwfcst.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

else

echo "   SKIPPING build_hiresw_wrfarwfcst"


fi

##############################

if [ $BUILD_hiresw_bucket -eq 1 ]
then
echo "executing build_hiresw_bucket"
./build_hiresw_bucket.sh >& ${logs_dir}/build_hiresw_bucket.log
echo hiresw_bucket.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

else 

echo "   SKIPPING build_hiresw_bucket"

fi


##############################

if [ $BUILD_hiresw_wrfbufr -eq 1 ]
then

echo "executing build_hiresw_wrfbufr"
./build_hiresw_wrfbufr.sh >& ${logs_dir}/build_hiresw_wrfbufr.log

echo hiresw_wrfbufr.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

else

echo "   SKIPPING build_hiresw_wrfbufr"

fi

##############################

if [ $BUILD_hiresw_wps -eq 1 ]
then

echo "executing build_hiresw_wps"
./build_hiresw_wps.sh >& ${logs_dir}/build_hiresw_wps.log
echo hiresw_wps.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
else

echo "   SKIPPING build_hiresw_wps"
fi

##############################

if [ $BUILD_hiresw_post -eq 1 ]
then

echo "executing build_hiresw_post"
./build_hiresw_post.sh >& ${logs_dir}/build_hiresw_post.log
echo hiresw_post.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
else

echo "   SKIPPING build_hiresw_post"

fi

##############################

if [ $BUILD_hiresw_smartinitg2 -eq 1 ]
then

echo "executing build_hiresw_smartinitg2"
./build_hiresw_smartinitg2.sh >& ${logs_dir}/build_hiresw_smartinitg2.log
echo hiresw_smartinitg2.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
else

echo "   SKIPPING build_hiresw_smartinitg2"

fi

##############################

if [ $BUILD_hiresw_smartprecipg2 -eq 1 ]
then

echo "executing build_hiresw_smartprecipg2"
./build_hiresw_smartprecipg2.sh >& ${logs_dir}/build_hiresw_smartprecipg2.log
echo hiresw_smartprecipg2.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
else

echo "   SKIPPING build_hiresw_smartprecipg2"


fi

##############################

if [ $BUILD_hiresw_sndp -eq 1 ]
then

echo "executing build_hiresw_sndp"
./build_hiresw_sndp.sh >& ${logs_dir}/build_hiresw_sndp.log
echo hiresw_sndp.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
else

echo "   SKIPPING build_hiresw_sndp"

fi

##############################

if [ $BUILD_hiresw_stnmlist -eq 1 ]
then

echo "executing build_hiresw_stnmlist"
./build_hiresw_stnmlist.sh >& ${logs_dir}/build_hiresw_stnmlist.log

echo hiresw_stnmlist.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
else

echo "   SKIPPING build_hiresw_stnmlist"

fi

##############################
