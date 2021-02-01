#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:          hiresw_prdgen_5km_grid.sh
# Script description:   Interpolates CONUS domain output to grid 227
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:             Only run for the CONUS domain, it provides full CONUS
#                       output of a large set of fields at 5 km.
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - documentation block and cleanup
# 2019-11-06  Matthew Pyle - adapted for FV3 system
#

set -x

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
subpiece=$5

compress="c3 -set_bitmap 1"

reflag=1

mkdir -p ${DATA}/prdgen_5km_${subpiece}/${fhr}
cd ${DATA}/prdgen_5km_${subpiece}/${fhr}

DOMIN=${DOMIN_SMALL}${model}

modelout=$model

if [ $model = "arw" ]
then
modelout="arw"
reflag=0
fi

DOMOUT=${DOMIN_SMALL}${modelout}

if [ $DOMIN = "conusfv3" ]
then
  filenamthree="fv3.CONUS05"
  IM=1473
  JM=1025
fi

filedir=$DATA

export fhr
export tmmark=tm00

###############################################################
###############################################################
###############################################################

#
# make GRIB file with pressure data every 25 mb for EMC's FVS
# verification

if [ $DOMIN_SMALL = "conus" ]
then
cp $PARMfv3/hiresw_conus_awp5km.txt_${subpiece} regional_grid_extract.txt
else
cp $PARMfv3/hiresw_conus_awp5km.txt regional_grid_extract.txt
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
 echo in while
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

echo INPUT_DATA is $INPUT_DATA

$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 | grep -F -f regional_grid_extract.txt | $WGRIB2 -i -grib inputs.grb $INPUT_DATA/BGDAWP${fhr}.tm00
export err=$?; err_chk

$WGRIB2  inputs.grb  -set_grib_type ${compress} -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_bilin
export err=$?; err_chk

if [ $subpiece = "1" ]
then
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":(HINDEX|CSNOW|CICEP|CFRZR|CRAIN|RETOP|REFD|LTNG|MAXREF):" -grib nn.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":TSOIL:0-0.1 m below ground:" -grib nn.grb_2
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":SOILW:0-0.1 m below ground:" -grib nn.grb_3
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match "HGT:cloud ceiling:" -grib ceiling.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":MDIV:30-0 mb above ground:" -grib mconv.grb
export err=$?; err_chk

cat nn.grb nn.grb_2 nn.grb_3  ceiling.grb mconv.grb  > inputs_nn.grb
rm nn.grb nn.grb_2 nn.grb_3  ceiling.grb  mconv.grb

$WGRIB2  inputs_nn.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_nn
export err=$?; err_chk

$WGRIB2  inputs_budget.grb -new_grid_interpolation budget -set_grib_type ${compress} -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk

rm inputs_nn.grb inputs_budget.grb

fi


if [ $subpiece = "1" ]
then
cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
else
mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
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
#      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2_${subpiece}
      cp ${filenamthree}${fhr}.tm00 $DATA/${RUN}.t${CYC}z.${model}_5km.f${fhr}.conus.grib2_${subpiece}
#  fi
else

# if [ $subpiece = "1" ]
# then
# echo  NOT COMPUTING PRECIP BUCKETS

cp ${filenamthree}${fhr}.tm00 ${RUN}.t${CYC}z.${model}_5km.f${fhr}.conus.grib2

# fi # subpiece=1

###### DONE PRECIP BUCKET

#  if test $SENDCOM = 'YES'
#  then
#     cp $DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2_${subpiece}
     cp ${RUN}.t${CYC}z.${model}_5km.f${fhr}.conus.grib2 $DATA/${RUN}.t${CYC}z.${model}_5km.f${fhr}.conus.grib2_${subpiece}
#  fi

fi # if f00 test

echo  "done" > $DATA/done_conus_5km_${subpiece}_f${fhr}
