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
 ${CP} hireswfv3_forecast.fd/NEMS/exe/NEMS.x            $EXEdir/hireswfv3_forecast.x

#------------------------------------
# install post
#------------------------------------
 ${CP} hireswfv3_post.fd/exec/ncep_post                 $EXEdir/hireswfv3_post.x

#------------------------------------
# install bufrpost
#------------------------------------
 ${CP} hireswfv3_bufr.fd/hireswfv3_bufr.x                 $EXEdir/hireswfv3_bufr.x

#------------------------------------
# install sndp
#------------------------------------
 ${CP} hireswfv3_sndp.fd/hireswfv3_sndp.x                 $EXEdir/hireswfv3_sndp.x

#------------------------------------
# install stnmlist
#------------------------------------
 ${CP} hireswfv3_stnmlist.fd/hireswfv3_stnmlist.x                 $EXEdir/hireswfv3_stnmlist.x

#------------------------------------
# install smartinit
#------------------------------------
 ${CP} hireswfv3_smartinit.fd/hireswfv3_smartinit                 $EXEdir/hireswfv3_smartinit.x

#------------------------------------
# install smartprecip
#------------------------------------
 ${CP} hireswfv3_smartprecip.fd/hireswfv3_smartprecip                 $EXEdir/hireswfv3_smartprecip.x

#------------------------------------
# install fv3snowbucket
#------------------------------------
 ${CP} hireswfv3_fv3snowbucket.fd/hireswfv3_fv3bucket                 $EXEdir/hireswfv3_fv3snowbucket.x

#------------------------------------
# install bucket
#------------------------------------
 ${CP} hireswfv3_bucket.fd/hireswfv3_bucket                 $EXEdir/hireswfv3_bucket.x



#------------------------------------
#------------------------------------
#------------------------------------
# install chgres
#------------------------------------
#  ${CP} hireswfv3_utils.fd/exec/global_chgres            $EXEdir/hireswfv3_chgres.x

#------------------------------------
# install chgres_cube
#------------------------------------
 ${CP} hireswfv3_utils.fd/exec/chgres_cube.exe          $EXEdir/hireswfv3_chgres_cube.x

#------------------------------------
# install orog
#------------------------------------
 ${CP} hireswfv3_utils.fd/exec/orog.x                   $EXEdir/hireswfv3_orog.x

#------------------------------------
# install sfc_climo_gen
#------------------------------------
 ${CP} hireswfv3_utils.fd/exec/sfc_climo_gen            $EXEdir/hireswfv3_sfc_climo_gen.x

#------------------------------------
# install hireswfv3_grid
#------------------------------------
#  ${CP} hireswfv3_utils.fd/exec/hireswfv3_grid            $EXEdir/hireswfv3_grid.x

#------------------------------------
# install fre-nctools
#------------------------------------
# ${CP} hireswfv3_utils.fd/exec/make_hgrid               $EXEdir/hireswfv3_make_hgrid.x
#${CP} hireswfv3_utils.fd/exec/make_hgrid_parallel      $EXEdir/hireswfv3_make_hgrid_parallel.x
# ${CP} hireswfv3_utils.fd/exec/make_solo_mosaic         $EXEdir/hireswfv3_make_solo_mosaic.x
# ${CP} hireswfv3_utils.fd/exec/fregrid                  $EXEdir/hireswfv3_fregrid.x
#${CP} hireswfv3_utils.fd/exec/fregrid_parallel         $EXEdir/hireswfv3_fregrid_parallel.x
# ${CP} hireswfv3_utils.fd/exec/filter_topo              $EXEdir/hireswfv3_filter_topo.x
# ${CP} hireswfv3_utils.fd/exec/shave.x                  $EXEdir/hireswfv3_shave.x


echo;echo " .... Install system finished .... "

exit 0
