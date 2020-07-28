#!/bin/ksh
############################################################################
# Script name:		exfv3cam_post.sh
# Script description:	Run the post processor jobs to create grib2 output.
# Script history log:
#   1) 2018-08-20	Eric Aligo / Hui-Ya Chuang
#                       Created script to post process output 
#                       from the SAR-FV3.
#   2) 2018-08-23	Ben Blake
#                       Adapted script into EE2-compliant Rocoto workflow.
#   3) 2019-11-06       Matthew Pyle
#                       Establishing an "odd/even" post script in case NCO wants it like
#                       current HiresW
############################################################################
set -x

fhr=01
fhrend=59
INCR=02

MODEL=fv3


# see if any postdone files exist, and if so, which is last hour completed


while [ $fhr -le $fhrend ]
do

incr=2

if [ -e postdone${fhr} ]
then
let fhr=fhr+incr

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

echo STARTING POST with fhr $fhr

while [ $fhr -le $fhrend ]
do

looplim=45
loop=1
while [ $loop -le $looplim ]
do
 echo in while
 if [ -s $INPUT_DATA/logf0${fhr} ]
 then
   break
 else
   loop=$((loop+1))
   sleep 20
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 15 minutes of waiting for $INPUT_DATA/logf0${fhr}"
   err_exit $msg
 fi
done

if [ $tmmark = tm00 ] ; then
  export NEWDATE=`${NDATE} +${fhr} $CDATE`
else
  offset=`echo $tmmark | cut -c 3-4`
  export vlddate=`${NDATE} -${offset} $CDATE`
  export NEWDATE=`${NDATE} +${fhr} $vlddate`
fi
export YYYY=`echo $NEWDATE | cut -c1-4`
export MM=`echo $NEWDATE | cut -c5-6`
export DD=`echo $NEWDATE | cut -c7-8`
export HH=`echo $NEWDATE | cut -c9-10`

cat > itag <<EOF
${INPUT_DATA}/dynf0${fhr}.nc
netcdf
grib2
${YYYY}-${MM}-${DD}_${HH}:00:00
FV3R
${INPUT_DATA}/phyf0${fhr}.nc

 &NAMPGB
 KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,
 /
EOF

rm -f fort.*

# copy flat files
cp ${PARMfv3}/nam_micro_lookup.dat      ./eta_micro_lookup.dat
cp ${PARMfv3}/postxconfig-NT-fv3sar.txt ./postxconfig-NT.txt
cp ${PARMfv3}/params_grib2_tbl_new      ./params_grib2_tbl_new

# Run the post processor
export pgm=hireswfv3_post.x
. prep_step

startmsg
${APRUNC} ${POSTGPEXEC} < itag > $pgmout 2> err
export err=$?; err_chk

# Run wgrib2

# echo done > postdone${fhr}

fhr=`expr $fhr + $INCR`

if [ $fhr -lt 10 ]
then
fhr=0$fhr
fi


done

echo EXITING $0

exit
