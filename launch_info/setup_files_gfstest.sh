

testcyc=2019080900


getcyc=`$NDATE -6 ${testcyc}`


PDY=`echo $getcyc | cut -c1-8`
cyc=`echo $getcyc | cut -c9-10`


echo GET FOR PDY $PDY
echo FOR cyc $cyc


BASE=/gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/com/gfs/prod/

return=`pwd`


para_hours="06 12 18 24 30 36 42 48 54"

prod_hours="09 15 21 27 33 39 45 51 24 30 36 42 48 54"


cd $BASE

mkdir -p gfs.${PDY}/${cyc}

cd  gfs.${PDY}/${cyc}

for hr in $para_hours
do
# scp prodwcoss:/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/prfv3rt3b/gfs.${PDY}/${cyc}/gfs.t${cyc}z.pgrb2.0p25.f0${hr} .
cp /gpfs/dell3/ptmp/emc.glopara/ROTDIRS/prfv3rt3b/gfs.${PDY}/${cyc}/gfs.t${cyc}z.pgrb2.0p25.f0${hr} .
done

for hr in $prod_hours
do
cp $COMROOTp3/gfs/prod/gfs.${PDY}/${cyc}/gfs.t${cyc}z.pgrb2.0p25.f0${hr} .
echo copied $COMROOTp3/gfs/prod/gfs.${PDY}/${cyc}/gfs.t${cyc}z.pgrb2.0p25.f0${hr}
done

cd $return
