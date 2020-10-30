#!/bin/sh
set -xeu

build_dir=`pwd`

CP='cp -rp'

EXEdir=../../exec/fv3

# Check final exec folder exists
if [ ! -d $EXEdir ]; then
  echo "Creating $EXEdir folder"
  mkdir -p $EXEdir
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

#------------------------------------
# install forecast
#------------------------------------
 ${CP} hireswfv3_forecast.fd/NEMS/exe/NEMS.x            $EXEdir/hireswfv3_forecast

#------------------------------------
# install post
#------------------------------------
 ${CP} hireswfv3_post.fd/exec/ncep_post                 $EXEdir/hireswfv3_post

#------------------------------------
# install bufrpost
#------------------------------------
 ${CP} hireswfv3_bufr.fd/hireswfv3_bufr.x                 $EXEdir/hireswfv3_bufr

#------------------------------------
# install sndp
#------------------------------------
 ${CP} hireswfv3_sndp.fd/hireswfv3_sndp.x                 $EXEdir/hireswfv3_sndp

#------------------------------------
# install stnmlist
#------------------------------------
 ${CP} hireswfv3_stnmlist.fd/hireswfv3_stnmlist.x                 $EXEdir/hireswfv3_stnmlist

#------------------------------------
# install smartinit
#------------------------------------
 ${CP} hireswfv3_smartinit.fd/hireswfv3_smartinit                 $EXEdir/hireswfv3_smartinit

#------------------------------------
# install smartprecip
#------------------------------------
 ${CP} hireswfv3_smartprecip.fd/hireswfv3_smartprecip                 $EXEdir/hireswfv3_smartprecip

#------------------------------------
# install fv3snowbucket
#------------------------------------
 ${CP} hireswfv3_fv3snowbucket.fd/hireswfv3_fv3bucket                 $EXEdir/hireswfv3_fv3snowbucket

#------------------------------------
# install bucket
#------------------------------------
 ${CP} hireswfv3_bucket.fd/hireswfv3_bucket                 $EXEdir/hireswfv3_bucket

#------------------------------------
# install chgres_cube
#------------------------------------
 ${CP} hireswfv3_utils.fd/exec/chgres_cube          $EXEdir/hireswfv3_chgres_cube

#------------------------------------
# install sfc_climo_gen
#------------------------------------
#  ${CP} hireswfv3_utils.fd/exec/sfc_climo_gen            $EXEdir/hireswfv3_sfc_climo_gen


echo;echo " .... Install system finished .... "

exit 0
