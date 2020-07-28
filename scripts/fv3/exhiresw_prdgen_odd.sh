#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhiresw_prdgen.sh
# Script description:  Run hiresw product generator jobs
#
# Author:        Eric Rogers       Org: NP22         Date: 1999-06-23
#
# Abstract: This script runs the HiresW PRDGEN jobs
#
# Script history log:
# 1999-06-23  Eric Rogers
# 1999-08-25  Brent Gordon  Modified for production, removed here file.
# 2003-03-21  Eric Rogers  Modified for special hourly output
# 2007-04-30  Matthew Pyle - Adopted for HiresW use
# 2008-05-06  Chris Magee - Add prdgendone file for gempak step to key on.
# 2009-09-24  Shawna Cokley - Eliminates copy of date file to working directory
# 2012-12-13  Matthew Pyle - Small changes for WCOSS machine
# 2013-09-01  Matthew Pyle - Changes to be run from prdgenmgr (works on the $post_times hour passed in)
# 2014-02-27  Matthew Pyle - Undoes changes to be run from prdgenmgr (now loops over time again)
# 2014-12-09  Matthew Pyle - Adds ability to be restarted midstream
# 2019-11-18  Matthew Pyle - Adapated for FV3 system
#

set -x

######
export INCR=02
######


cd $DATA
filedir=$DATA

export tmmark=tm00

export fhr=01
export fhrend=59

## see if any prdgendone?? files exist, and if so, which is the last hour completed

while [ $fhr -le $fhrend ]
do

if [ -e prdgendone${fhr} ]
then
let fhr=fhr+INCR

if [ $fhr -lt 10 ]
then
fhr=0$fhr
fi

fhrsave=${fhr}

else
fhrsave=${fhr}
fhr=99
fi

done

fhr=${fhrsave}

echo STARTING PRDGEN with fhr $fhr

icnt=1

while [ $fhr -le $fhrend ]
do

# is post done for this hour?

if [ $fhr -eq 00 ]
then
INPUT_DATA=$INPUT_DATA_EVEN
elif [ $fhr%2 -eq 0 ]
then
INPUT_DATA=$INPUT_DATA_EVEN
else
INPUT_DATA=$INPUT_DATA_ODD
fi


echo to here with INPUT_DATA $INPUT_DATA

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


################################################
if [ $NEST = "conus" ]
then
################################################


# put USH calls in here

echo "#!/bin/bash" > $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_big_grid_g2.sh $fhr $NEST $cyc $MODEL 1 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_big_grid_g2.sh $fhr $NEST $cyc $MODEL 2 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_big_grid_g2.sh $fhr $NEST $cyc $MODEL 3 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_5km_grid_g2.sh     $fhr $NEST $cyc $MODEL 1 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_5km_grid_g2.sh     $fhr $NEST $cyc $MODEL 2 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_5km_grid_g2.sh     $fhr $NEST $cyc $MODEL 3 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_5km_grid_g2.sh     $fhr $NEST $cyc $MODEL 4 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_5km_grid_g2.sh     $fhr $NEST $cyc $MODEL 5 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_5km_grid_g2.sh     $fhr $NEST $cyc $MODEL 6 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh_5km $fhr $NEST $cyc $MODEL 1 conus &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh_5km $fhr $NEST $cyc $MODEL 2 conus &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_3km_grid_g2.sh     $fhr $NEST $cyc $MODEL 1 &" >> $DATA/poescript_${fhr}
echo "wait" >> $DATA/poescript_${fhr}
chmod 775 $DATA/poescript_${fhr}
command="aprun -n 1 -N 1 -d 12 $DATA/poescript_${fhr} "

time $command
export err=$?; err_chk

# reassemble the large 5 km output grid


  if test $SENDCOM = 'YES'
  then

cat $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_1 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_2 \
    $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_3 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_4 \
    $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_5 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_6 > \
    $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2

$WGRIB2 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2 -ncep_uv $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2
$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2 -s >   $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2.idx


## subset the full CONUS grid to approximate the CAM common grid list
## now likely will interpolate subset products to 3 km grid instead for CONUS

### change to use a PARM .txt file, and do for CONUS and AK

# cp $PARMfv3/hiresw_subset.txt .

# $WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2 |  grep -F -f hiresw_subset.txt | \
# $WGRIB2  -i -grib $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.subset.grib2  $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2

cat $DATA/${RUN}.t${cyc}z.${MODEL}_3km.f${fhr}.conus.grib2_1  > $COMOUT/${RUN}.t${cyc}z.${MODEL}_3km.f${fhr}.conus.subset.grib2
$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_3km.f${fhr}.conus.subset.grib2 -s > $COMOUT/${RUN}.t${cyc}z.${MODEL}_3km.f${fhr}.conus.subset.grib2.idx


    if [ $SENDDBN = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE}      $job $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_WIDX} $job $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2.idx
    fi

  fi # SENDCOM

rm  $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_1 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_2 \
    $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_3 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_4 \
    $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_5 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_6

# reassemble the ndfd output grid

cat $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.${NEST}.grib2_1  $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.${NEST}.grib2_2 \
    $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.${NEST}.grib2_3 > $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.${NEST}.grib2




# is this overwritten by smartinit output?
#  if test $SENDCOM = 'YES'
#  then
# cp $DATA/prdgen_full/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.conus.grib2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.conus.grib2
#   fi

rm $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.conus.grib2_1
rm $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.conus.grib2_2
rm $DATA/${RUN}.t${cyc}z.${MODEL}_2p5km.f${fhr}.conus.grib2_3

# reassemble the legacy 5 km output grid

  if test $SENDCOM = 'YES'
  then

cat $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_conus_1 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_conus_2 > $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_sbn

$WGRIB2 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_sbn -ncep_uv $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_sbn
$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_sbn -s > $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.conus.grib2_sbn.idx

  fi



################################################
elif [ $NEST = "ak" ]
then
################################################

echo "#!/bin/bash" > $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_big_grid_g2.sh $fhr $NEST $cyc $MODEL 0 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh $fhr $NEST $cyc $MODEL 1 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh $fhr $NEST $cyc $MODEL 2 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh $fhr $NEST $cyc $MODEL 3 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh $fhr $NEST $cyc $MODEL 4 &" >> $DATA/poescript_${fhr}
echo "wait" >> $DATA/poescript_${fhr}
chmod 775 $DATA/poescript_${fhr}
command="aprun -n 1 -N 1 -d 5 $DATA/poescript_${fhr} "

time $command
export err=$?; err_chk


  if test $SENDCOM = 'YES'
  then

cat $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_1 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_2 \
    $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_3 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_4 > \
  $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2

$WGRIB2 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2 -ncep_uv $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2

rm $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_1 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_2 \
   $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_3 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_4

$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2 -s > $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2.idx

    if [ $SENDDBN = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE}      $job $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_WIDX} $job $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2.idx
    fi


cp $PARMfv3/hiresw_subset.txt .

$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2 |  grep -F -f hiresw_subset.txt | \
$WGRIB2  -i -grib $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.subset.grib2  $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2


$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.subset.grib2 -s > $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.subset.grib2.idx

  fi

################################################
else
################################################

echo "#!/bin/bash" > $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_big_grid_g2.sh $fhr $NEST $cyc $MODEL 0 &" >> $DATA/poescript_${fhr}
echo "$USHfv3/hiresw_prdgen_oldgrid_g2.sh $fhr $NEST $cyc $MODEL 0 &" >> $DATA/poescript_${fhr}
echo "wait" >> $DATA/poescript_${fhr}
chmod 775 $DATA/poescript_${fhr}
command="aprun -n 1 -N 1 -d 2 $DATA/poescript_${fhr} "

time $command
export err=$?; err_chk


  if test $SENDCOM = 'YES'
  then

echo here with DATA $DATA

ls -l  $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_0
$WGRIB2 $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_0 -ncep_uv $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2
$WGRIB2 $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2 -s > $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2.idx

rm $DATA/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2_0

    if [ $SENDDBN = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE}      $job $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_WIDX} $job $COMOUT/${RUN}.t${cyc}z.${MODEL}_5km.f${fhr}.${NEST}.grib2.idx
    fi

  fi

fi

export err=$?; err_chk

if [ $SENDCOM = YES ]
then

  if [ $tmmark = tm00 ] ; then
#    mv ${NEST}${RUN}.f${fhr}.${tmmark} ${COMOUT}/${RUN}.t${cyc}z.${NEST}.f${fhr}.grib2
    cp $INPUT_DATA/BGDAWP${fhr}.${tmmark} ${COMOUT}/${RUN}.t${cyc}z.${NEST}.natprs.f${fhr}.grib2
  else
#    mv ${NEST}${RUN}.f${fhr}.${tmmark} ${COMOUT}/${RUN}.t${cyc}z.${NEST}.f${fhr}.${tmmark}.grib2
    cp $INPUT_DATA/BGDAWP${fhr}.${tmmark} ${COMOUT}/${RUN}.t${cyc}z.${NEST}.natprs.f${fhr}.${tmmark}.grib2
  fi

fi

echo "done executing prdgen" > $DATA/prdgendone${fhr}
postmsg "$jlogfile" "HIRESW ${NEST}${MODEL} PRDGEN done for F${fhr}"

let fhr=fhr+INCR

if [ $fhr -lt 10 ]
then
fhr=0$fhr
fi

done
