#! /bin/ksh

dom=conus

YR=2019
MO=11
DY=01
CYC=00

CDATE=$YR$MO$DY$CYC

RUNDIR=/gpfs/hps2/stmp/Matthew.Pyle/oconus_new/tmpnwprd/regional_forecast_tm00_${dom}_${CDATE}

cd $RUNDIR
cp /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/regional_workflow_new/exec/hiresw_wrfbufr .

hr=01
hrlim=60

DO_FIRST=1

if [ DO_FIRST -eq 1 ]
then
rm test_profilm.all

while [ $hr -le $hrlim ]
do

rm test_profilm_${hr}


echo hr at top of loop is $hr

echo dynf0${hr}.nc > itag
echo phyf0${hr}.nc >> itag
echo FV3S >> itag
echo netcdf >> itag
echo ${YR}-${MO}-${DY}:${CYC}:00 >> itag
echo 1 >> itag
echo 1 >> itag
echo $hr >> itag
echo dynf0${hr}.nc >> itag
echo phyf0${hr}.nc >> itag

ln -sf itag fort.11


ln -sf /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/regional_workflow_new/fix/fix_sar/hiresw_${dom}fv3sar_profdat fort.19
ln -sf test_profilm_${hr} fort.79

./hiresw_wrfbufr

cat test_profilm_${hr} >> test_profilm.all

let hr=hr+1

echo hr now $hr

if [ $hr -lt 10 ]
then
hr=0${hr}
fi

done

fi  # DO_FIRST

# ---------------------------------------------------------

rm fort.*

cp /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/regional_workflow_new/parm/hiresw_bufr.tbl .
cp /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/regional_workflow_new/parm/hiresw_sndp.parm.mono .

ln -sf  hiresw_sndp.parm.mono fort.11
ln -sf hiresw_bufr.tbl fort.32
ln -sf test_profilm.all fort.66
ln -sf class1.bufr fort.78

nlev=60

cp /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/regional_workflow_new/exec/hiresw_sndp .
echo "FV3 $nlev" > itag
# aprun $EXEChiresw/hiresw_sndp < itag >> $pgmout 2>$pgmout
./hiresw_sndp < itag 
export err=$?;err_chk

