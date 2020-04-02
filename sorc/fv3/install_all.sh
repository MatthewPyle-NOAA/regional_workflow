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
 ${CP} regional_forecast.fd/NEMS/exe/NEMS.x            $EXEdir/regional_forecast.x

#------------------------------------
# install post
#------------------------------------
 ${CP} regional_post.fd/exec/ncep_post                 $EXEdir/regional_post.x

#------------------------------------
# install bufrpost
#------------------------------------
 ${CP} regional_bufr.fd/regional_bufr.x                 $EXEdir/regional_bufr.x

#------------------------------------
# install sndp
#------------------------------------
 ${CP} regional_sndp.fd/regional_sndp.x                 $EXEdir/regional_sndp.x

#------------------------------------
# install stnmlist
#------------------------------------
 ${CP} regional_stnmlist.fd/regional_stnmlist.x                 $EXEdir/regional_stnmlist.x

#------------------------------------
# install smartinit
#------------------------------------
 ${CP} regional_smartinit.fd/regional_smartinit                 $EXEdir/regional_smartinit.x

#------------------------------------
# install smartprecip
#------------------------------------
 ${CP} regional_smartprecip.fd/regional_smartprecip                 $EXEdir/regional_smartprecip.x

#------------------------------------
# install fv3snowbucket
#------------------------------------
 ${CP} regional_fv3snowbucket.fd/regional_fv3bucket                 $EXEdir/regional_fv3snowbucket.x

#------------------------------------
# install bucket
#------------------------------------
 ${CP} regional_bucket.fd/regional_bucket                 $EXEdir/regional_bucket.x



#------------------------------------
#------------------------------------
#------------------------------------
# install chgres
#------------------------------------
#  ${CP} regional_utils.fd/exec/global_chgres            $EXEdir/regional_chgres.x

#------------------------------------
# install chgres_cube
#------------------------------------
 ${CP} regional_utils.fd/exec/chgres_cube.exe          $EXEdir/regional_chgres_cube.x

#------------------------------------
# install orog
#------------------------------------
 ${CP} regional_utils.fd/exec/orog.x                   $EXEdir/regional_orog.x

#------------------------------------
# install sfc_climo_gen
#------------------------------------
 ${CP} regional_utils.fd/exec/sfc_climo_gen            $EXEdir/regional_sfc_climo_gen.x

#------------------------------------
# install regional_grid
#------------------------------------
#  ${CP} regional_utils.fd/exec/regional_grid            $EXEdir/regional_grid.x

#------------------------------------
# install fre-nctools
#------------------------------------
# ${CP} regional_utils.fd/exec/make_hgrid               $EXEdir/regional_make_hgrid.x
#${CP} regional_utils.fd/exec/make_hgrid_parallel      $EXEdir/regional_make_hgrid_parallel.x
# ${CP} regional_utils.fd/exec/make_solo_mosaic         $EXEdir/regional_make_solo_mosaic.x
# ${CP} regional_utils.fd/exec/fregrid                  $EXEdir/regional_fregrid.x
#${CP} regional_utils.fd/exec/fregrid_parallel         $EXEdir/regional_fregrid_parallel.x
# ${CP} regional_utils.fd/exec/filter_topo              $EXEdir/regional_filter_topo.x
# ${CP} regional_utils.fd/exec/shave.x                  $EXEdir/regional_shave.x


echo;echo " .... Install system finished .... "

exit 0
