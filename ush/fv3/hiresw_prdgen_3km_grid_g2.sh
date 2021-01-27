#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:          hiresw_prdgen_3km_grid.sh
# Script description:   Interpolates CONUS domain output to 3 km grid
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:             Only run for the CONUS domain, it provides subset of CONUS
#                       output on the 3km HRRR grid.
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - documentation block and cleanup
# 2017-02-16  Matthew Pyle - modified hiresw_prdgen_5km_grid.sh for 3 km.
# 2019-11-14  Matthew Pyle - modified for FV3 system



set -x

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
subpiece=$5

wgrib2def="lambert:262.5:38.5:38.5 237.280:1799:3000 21.138:1059:3000"

compress="c3 -set_bitmap 1"

reflag=1

mkdir -p ${DATA}/prdgen_3km_${subpiece}/${fhr}
cd ${DATA}/prdgen_3km_${subpiece}/${fhr}/

DOMIN=${DOMIN_SMALL}${model}

modelout=$model
if [ $model = "arw" ]
then
modelout="arw"
reflag=0
fi

DOMOUT=${DOMIN_SMALL}${modelout}

if [ $DOMIN_SMALL = "conus" ]
then
  filenamthree="fv3.CONUS03"
  IM=1799
  JM=1059
fi

filedir=$DATA

export fhr
export tmmark=tm00


###############################################################
###############################################################
###############################################################

#

if [ $DOMIN_SMALL = "conus" ]
then
cp $PARMfv3/hiresw_subset.txt hiresw_grid_extract.txt
fi

# if [ $DOMIN_SMALL = "conus" ]
# then

# if [ $fhr -eq 00 ]
# then
# INPUT_DATA=$INPUT_DATA_EVEN
# elif [ $fhr%2 -eq 0 ]
# then
# INPUT_DATA=$INPUT_DATA_EVEN
# else
# INPUT_DATA=$INPUT_DATA_ODD
# fi

# INPUT_DATA_FORE=${INPUT_DATA}

# $DATA should be post working directory

# INPUT_DATA=${DATA}

# fi



looplim=90
loop=1
while [ $loop -le $looplim ]
do
 if [ -s $INPUT_DATA/postdone${fhr} ]
 then
   break
 else
  loop=$((loop+1))
  sleep 20
 fi

 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 30 minutes of waiting for $INPUT_DATA/postdone${fhr}"
   err_exit $msg
 fi

done

### extract just needed items

$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 | grep -F -f hiresw_grid_extract.txt | $WGRIB2 -i -grib inputs.grb $INPUT_DATA/BGDAWP${fhr}.tm00
 export err=$?; err_chk
$WGRIB2  inputs.grb  -set_grib_type ${compress} -new_grid_interpolation neighbor -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn
 export err=$?; err_chk


if [ $subpiece = "1" ]
then
cat ${filenamthree}${fhr}.tm00_nn > ${filenamthree}${fhr}.tm00
else
mv ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00
fi

export err=$?; err_chk


###############################################################
###############################################################
###############################################################


### NIX all bucket stuff for FV3SAR??


echo "to f00 test"

if [ $fhr -eq 00 ]
then
echo "inside f00 test"

  ###############################
  # Convert to grib2 format
  ###############################

#  if test $SENDCOM = 'YES'
#  then
      cp ${filenamthree}${fhr}.tm00 $DATA/${RUN}.t${CYC}z.${model}_3km.f${fhr}.conus.grib2_${subpiece}
#  fi
else

if [ $subpiece = "1" ]
then
echo  NOT COMPUTING PRECIP BUCKETS


	

cp ${filenamthree}${fhr}.tm00 ${RUN}.t${CYC}z.${model}_3km.f${fhr}.conus.grib2

fi # subpiece=1

###### DONE PRECIP BUCKET

#  if test $SENDCOM = 'YES'
#  then
     cp ${RUN}.t${CYC}z.${model}_3km.f${fhr}.conus.grib2 $DATA/${RUN}.t${CYC}z.${model}_3km.f${fhr}.conus.grib2_${subpiece}
#  fi

fi # if f00 test

echo  "done" > $DATA/done_conus_3km_${subpiece}_f${fhr}
