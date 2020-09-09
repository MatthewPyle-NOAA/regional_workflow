set -x
module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load ../modulefiles/HIRESW/v8.0.0
module list


##################################

# setting a INSTALL* variable to 1 means
# that build script will run.
# 
# setting a INSTALL* variable to 0 (technically anything but 1)
# will skip the build of that component.

INSTALL_hiresw_wrfarwfcst=1
INSTALL_hiresw_bucket=1
INSTALL_hiresw_wrfbufr=1
INSTALL_hiresw_wps=1
INSTALL_hiresw_post=1
INSTALL_hiresw_smartinitg2=1
INSTALL_hiresw_smartprecipg2=1
INSTALL_hiresw_sndp=1
INSTALL_hiresw_stnmlist=1

EXEdir=../../exec/arw

if [ ! -d $EXEdir ]; then
  echo "Creating $EXEdir folder"
  mkdir $EXEdir
fi


##################################
#
# SHOULD REQUIRE NO CHANGES BELOW
#
##################################

export BASE=`pwd`

CP='cp -rp'


if [ ! -e $BASE/logs ]
then
mkdir $BASE/logs
fi

export logs_dir=$BASE/logs

set +x

if [ $INSTALL_hiresw_wrfarwfcst -eq 1 ]
then

${CP} ./hiresw_wrfarwfcst.fd/main/real.exe $EXEdir/hiresw_wrfarwfcst_init
${CP} ./hiresw_wrfarwfcst.fd/main/wrf.exe $EXEdir/hiresw_wrfarwfcst

else

echo "   SKIPPING install_hiresw_wrfarwfcst"


fi

##############################

if [ $INSTALL_hiresw_bucket -eq 1 ]
then
# echo "executing install_hiresw_bucket"
# ./install_hiresw_bucket.sh >& ${logs_dir}/install_hiresw_bucket.log
# echo hiresw_bucket.fd done XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

${CP} hiresw_bucket.fd/hiresw_bucket $EXEdir/

else 

echo "   SKIPPING install_hiresw_bucket"

fi


##############################

if [ $INSTALL_hiresw_wrfbufr -eq 1 ]
then


${CP} hiresw_wrfbufr.fd/hiresw_wrfbufr $EXEdir/

else

echo "   SKIPPING install_hiresw_wrfbufr"

fi

##############################

if [ $INSTALL_hiresw_wps -eq 1 ]
then


${CP} hiresw_wps.fd/ungrib/src/ungrib.exe $EXEdir/hiresw_wps_ungrib
${CP} hiresw_wps.fd/metgrid/src/metgrid.exe $EXEdir/hiresw_wps_metgrid

else

echo "   SKIPPING install_hiresw_wps"
fi

##############################

if [ $INSTALL_hiresw_post -eq 1 ]
then


${CP} hiresw_post.fd/hiresw_post $EXEdir/

else

echo "   SKIPPING install_hiresw_post"

fi

##############################

if [ $INSTALL_hiresw_smartinitg2 -eq 1 ]
then


${CP} hiresw_smartinitg2.fd/hiresw_smartinitg2 $EXEdir/

else

echo "   SKIPPING install_hiresw_smartinitg2"

fi

##############################

if [ $INSTALL_hiresw_smartprecipg2 -eq 1 ]
then


${CP} hiresw_smartprecipg2.fd/hiresw_smartprecipg2 $EXEdir/

else

echo "   SKIPPING install_hiresw_smartprecipg2"


fi

##############################

if [ $INSTALL_hiresw_sndp -eq 1 ]
then

${CP} ./hiresw_sndp.fd/hiresw_sndp $EXEdir/

else

echo "   SKIPPING install_hiresw_sndp"

fi

##############################

if [ $INSTALL_hiresw_stnmlist -eq 1 ]
then

${CP} ./hiresw_stnmlist.fd/hiresw_stnmlist $EXEdir

else

echo "   SKIPPING install_hiresw_stnmlist"

fi

##############################
