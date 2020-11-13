#! /bin/bash

BASE=`pwd`

##############################

cd ${BASE}/hireswfv3_forecast.fd/FV3
make clean
make cleanall

##############################

cd ${BASE}/hireswfv3_bucket.fd
make clean

##############################

cd ${BASE}/hireswfv3_bufr.fd
make clean

##############################

cd ${BASE}/hireswfv3_fv3snowbucket.fd
clean -a

##############################

cd ${BASE}/hireswfv3_post.fd
make clean

##############################

cd ${BASE}/hireswfv3_smartinit.fd
make clean

##############################

cd ${BASE}/hireswfv3_smartprecip.fd
make clean

##############################

cd ${BASE}/hireswfv3_sndp.fd
make clean

##############################

cd ${BASE}/hireswfv3_stnmlist.fd
make clean

##############################

cd ${BASE}/hireswfv3_wgrib2.cd/grib2
make clean
make deep-clean

##############################
