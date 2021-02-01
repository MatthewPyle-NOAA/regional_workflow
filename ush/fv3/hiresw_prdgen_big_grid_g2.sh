#! /bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:           hiresw_prdgen_big_grid.sh
# Script description:    Runs prdgen for the highest resolution NDFD output from HIRESW
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:            Runs prdgen to horizontally interpolate a limited number
#                      of fields onto the 2.5 or 3 km NDFD grid associated with
#                      a specific region.
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - documentation block
# 2019-11-06  Matthew Pyle - adapted for FV3 system

set -x

# export WGRIB2=${WGRIB2:-${utilexec}/wgrib2}

fhr=$1
DOMIN_SMALL=$2
CYC=$3
model=$4
subpiece=${5}

reflag=1
compress="c3 -set_bitmap 1"

mkdir -p ${DATA}/prdgen_full

if [ $DOMIN_SMALL = "conus"  ]
then
 mkdir -p ${DATA}/prdgen_full_${subpiece}/${fhr}
 cd ${DATA}/prdgen_full_${subpiece}/${fhr}
else
 mkdir -p ${DATA}/prdgen_full/${fhr}
 cd ${DATA}/prdgen_full/${fhr}
fi

#cd $DATA
DOMIN=${DOMIN_SMALL}${model}

model=fv3

modelout=$model

DOMOUT=${DOMIN_SMALL}${modelout}
DOMOUTtwo="cent"${modelout}

if [ $DOMIN_SMALL = "conus" ]
then
  filenamthree="fv3.CONUS04"
  rg="conus"
  gres="2p5km"
  IM=2145
  JM=1377
  wgrib2def="lambert:265:25:25 238.446:2145:2540 20.192:1377:2540"
elif [ $DOMIN_SMALL = "ak" ]
then
  filenamthree="fv3.AK04"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
  wgrib2def="nps:210:60 181.429:1649:2976 40.53:1105:2976"
elif [ $DOMIN_SMALL = "guam" ]
then
  filenamthree="fv3.GU04"
  rg="guam"
  gres="2p5km"
  IM=193
  JM=193
  wgrib2def="mercator:20 143.687:193:2500:148.280 12.35:193:2500:16.794"
  echo set wgrib2def to $wgrib2def
elif [ $DOMIN_SMALL = "hi" ]
then
  filenamthree="fv3.HI04"
  rg="hi"
  gres="2p5km"
  IM=321
  JM=225
  wgrib2def="mercator:20 198.475:321:2500:206.131 18.073:225:2500:23.088"
elif [ $DOMIN_SMALL = "pr" ]
then
  filenamthree="fv3.PR04"
  rg="pr"
  gres="2p5km"
  IM=177
  JM=129
  wgrib2def="mercator:20 291.804:177:2500:296.028 16.829:129:2500:19.747"
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

# DATA should be post working directory

# INPUT_DATA=${DATA}

use_1h=0
use_3h=0

if [ $fhr -eq 0 ]
then


 if [ $DOMIN_SMALL = "conus"  ]
 then
 cp $PARMfv3/hiresw_ndfd.txt_3h_conus_${subpiece} regional_grid_extract.txt
 else
 cp $PARMfv3/hiresw_ndfd.txt_3h regional_grid_extract.txt
 fi

use_3h=1

elif [ $fhr%3 -eq 0 ]
then

 if [ $DOMIN_SMALL = "conus" ]
 then
 cp $PARMfv3/hiresw_ndfd.txt_3h_conus_${subpiece} regional_grid_extract.txt
 else
 cp $PARMfv3/hiresw_ndfd.txt_3h regional_grid_extract.txt
 fi

use_3h=1

else


 if [ $DOMIN_SMALL = "conus" ]
 then
 cp $PARMfv3/hiresw_ndfd.txt_1h_conus_${subpiece} regional_grid_extract.txt
 elif [ $DOMIN_SMALL = "guam"  -a $fhr -lt 24 ]
 then
 cp $PARMfv3/hiresw_ndfd.txt_3h_guam regional_grid_extract.txt
 else
 cp $PARMfv3/hiresw_ndfd.txt_1h regional_grid_extract.txt
 fi

use_1h=1

fi

# use *ndfd.txt_1h and *ndfd.txt_3h files?

echo use_1h $use_1h
echo use_3h $use_3h


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
export err=$?; err_chk

echo to line with wgrib2def $wgrib2def

$WGRIB2  inputs.grb  -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_bilin
export err=$?; err_chk

if [ $DOMIN_SMALL = "conus"  -a $subpiece = "1" ] 
then
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":HGT:cloud ceiling" -grib inputs_budget_b.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":HGT:cloud base" -grib inputs_budget_c.grb
export err=$?; err_chk
cat inputs_budget_b.grb >> inputs_budget.grb
cat inputs_budget_c.grb >> inputs_budget.grb
$WGRIB2 inputs_budget.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk
fi


if [ $DOMIN_SMALL != "conus" ] 
then
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":HGT:cloud ceiling" -grib inputs_budget_b.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match ":HGT:cloud base" -grib inputs_budget_c.grb
export err=$?; err_chk
cat inputs_budget_b.grb >> inputs_budget.grb
cat inputs_budget_c.grb >> inputs_budget.grb
$WGRIB2 inputs_budget.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk
fi


     if [ $fhr -ne 0 ]
     then
     check=$fhr%3
     else
     check=9
     fi

##########
     if [ $check -ne 0 ]
     then
##########

	echo inside the check block with DOMIN_SMALL $DOMIN_SMALL


#####
if [ $DOMIN_SMALL = "conus" ]
then

if [ $subpiece = "1" ]
then
  $WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match "HINDEX" -grib nn.grb
  export err=$?; err_chk
  $WGRIB2 nn.grb  -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn
  export err=$?; err_chk
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget  > ${filenamthree}${fhr}.tm00
else
  mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
fi

fi
#####

if [ $DOMIN_SMALL != "conus" ]
then
  $WGRIB2 $INPUT_DATA/BGDAWP${fhr}.tm00 -match "HINDEX" -grib nn.grb
  export err=$?; err_chk
  $WGRIB2 nn.grb  -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn
  export err=$?; err_chk

  echo doing the cat into ${filenamthree}${fhr}.tm00

  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget  > ${filenamthree}${fhr}.tm00
fi

##########
     else # 3 hour time
##########

	echo DOMIN_SMALL $DOMIN_SMALL 
	echo subpiece $subpiece

if [ $DOMIN_SMALL = "conus" ]
then

if [ $subpiece = "1" ]
then
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
else
  mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
fi

fi

if [ $DOMIN_SMALL != "conus" ]
then
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
fi

fi  # 3 hour time

export err=$?; err_chk


###############################################################
###############################################################
###############################################################


if [ $fhr -eq 00 ]
then

echo fhr is 00 what is DOMIN_SMALL $DOMIN_SMALL

if [ $DOMIN_SMALL = "conus"  ] 
then
	echo COPYING f00 file to ${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}
       cp ${filenamthree}${fhr}.tm00 ${DATA}/${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}
else
        echo COPYING f00 to ${DATA}/${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2
       cp ${filenamthree}${fhr}.tm00 ${DATA}/${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2
fi


### will be cat'd to smartinit generated file in ncoproc script
### no need to index, convert, or alert this file (which is not the final one for output)

  
else  # (not f00)

valcheck=`echo $DOMIN_SMALL | cut -c1-5`

if [ $subpiece = "1" -o $valcheck != "conus" ]
then

if [ $subpiece = "1" ]
then
looplim=90
loop=1
else
looplim=90
loop=1
fi


if [ $subpiece = "1" ]
then
looplim=90
loop=1
else
looplim=90
loop=1
fi

  rm input.card.${fhr}
if [ $subpiece = "1" ]
then
  echo "$DATA/prdgen_full_1" > input.card.${fhr}
else
  echo "$DATA/prdgen_full" > input.card.${fhr}
fi

     cat ${filenamthree}${fhr}.tm00  > ${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2

else

mv ${filenamthree}${fhr}.tm00 ${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2

fi # subpiece=1 or non-conus

###### DONE PRECIP BUCKET

fi # f00 or not

echo DOWN HERE

## temp copy to $DATA
	if [ $subpiece -gt 0 ]
        then
         echo copying a grib2 to DATA $DATA
         cp ${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2 \
	 ${DATA}/${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}
        else
	if [ -e ${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2 ] 
        then
         cp  ${RUN}.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2 ${DATA}/
	fi
        fi
## temp copy to $DATA

echo  "done" > $DATA/done_ndfd_${subpiece}_f${fhr}
