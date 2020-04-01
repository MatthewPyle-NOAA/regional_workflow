#! /bin/bash

BASE=`pwd`

##############################

cd ${BASE}/hiresw_wrfarwfcst.fd
clean -a

##############################

cd ${BASE}/hiresw_bucket.fd
make clean

##############################

cd ${BASE}/hiresw_wrfbufr.fd
make clean

##############################

cd ${BASE}/hiresw_wps.fd
clean -a

##############################

cd ${BASE}/hiresw_post.fd
make clean

##############################

cd ${BASE}/hiresw_smartinitg2.fd
make clean

##############################

cd ${BASE}/hiresw_smartprecipg2.fd
make clean

##############################

cd ${BASE}/hiresw_sndp.fd
make clean

##############################

cd ${BASE}/hiresw_stnmlist.fd
make clean

##############################
