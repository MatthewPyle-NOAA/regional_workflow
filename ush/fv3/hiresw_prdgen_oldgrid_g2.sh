#! /bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:            hiresw_prdgen_oldgrid.sh
# Script description:     Runs prdgen for the legacy (old) grid distributed to AWIPS
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:      Runs prdgen for a specific hour, domain, and model, horizontally interpolating
#                native GRIB output onto the legacy 5 km grids
#
# Script history log:
# 2013-08-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - Added brief docblock
# 2019-11-06  Matthew Pyle - Simplified version for FV3


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
# modelout="em"
fi

DOMOUT=${DOMIN_SMALL}${modelout}

if [ $DOMIN_SMALL = "ak" ]
then
  filenamthree="fv3.AK05"
  wgrib2def="nps:210:60 185.5:825:5000 44.8:603:5000"
elif [ $DOMIN_SMALL = "hi" ]
then
  filenamthree="fv3.HI05"
  wgrib2def="latlon 197.65:223:.045 16.4:170:.045"
elif [ $DOMIN_SMALL = "pr" ]
then
  filenamthree="fv3.PR05"
  wgrib2def="latlon 283.41:340:.045 13.5:208:.045"
elif [ $DOMIN_SMALL = "guam" ]
then
  filenamthree="fv3.GU05"
  wgrib2def="latlon 141.0:223:.045 11.7:170:.045"
elif [ $DOMIN_SMALL = "conus" ]
then
  filenamthree="fv3.CO05"
  wgrib2def="lambert:265:25:25 226.541:1473:5079 12.190:1025:5079"
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

if [ $DOMIN_SMALL = "ak" -o $DOMIN_SMALL = "akmem2" ]
then
cp $PARMfv3/hiresw_awpreg.txt_${subpiece} regional_grid_extract.txt
else
cp $PARMfv3/hiresw_awpreg.txt regional_grid_extract.txt
fi


# INPUT_DATA_FORE=${INPUT_DATA}

# $DATA should be post working directory

# INPUT_DATA=${DATA}

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

$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 | grep -F -f regional_grid_extract.txt | $WGRIB2 -i -grib inputs.grb $INPUT_DATA/BGDAWP${fhr}.tm00

$WGRIB2 inputs.grb -set_grib_type ${compress} -new_grid_vectors UGRD:VGRD -new_grid_winds grid -new_grid_interpolation neighbor -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_bilin

if [ $subpiece =  "0"  -o $subpiece =  "1" ]
then
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":(HINDEX|CSNOW|CICEP|CFRZR|CRAIN|RETOP|REFD|LTNG|MAXREF):" -grib nn.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":TSOIL:0-0.1 m below ground:" -grib nn.grb_2
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":SOILW:0-0.1 m below ground:" -grib nn.grb_3
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":MDIV:30-0 mb above ground:" -grib mconv.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match "HGT:cloud ceiling:" -grib ceiling.grb
export err=$?; err_chk

cat nn.grb nn.grb_2 nn.grb_3 mconv.grb   ceiling.grb > inputs_nn.grb

rm nn.grb nn.grb_2 nn.grb_3 mconv.grb ceiling.grb

$WGRIB2 inputs_nn.grb -set_grib_type ${compress} -new_grid_winds grid -new_grid_interpolation neighbor -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn
$WGRIB2 inputs_budget.grb -set_grib_type ${compress} -new_grid_winds grid -new_grid_interpolation budget -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_budget
fi

if [ $subpiece =  "0"  -o $subpiece =  "1" ]
then
cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
else
mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
fi


export err=$?; err_chk

###############################################################
###############################################################


echo "to f00 test"

if [ $fhr -eq 00 ]
then
echo "inside f00 test"

      cp ${filenamthree}${fhr}.tm00 $DATA/${RUN}.t${CYC}z.${model}_5km.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}

else

      cp ${filenamthree}${fhr}.tm00 $DATA/${RUN}.t${CYC}z.${model}_5km.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}

fi # fhour

###### DONE PRECIP BUCKET

