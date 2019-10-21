#!/bin/sh
############################################################################
# Script name:		exfv3cam_sar_chgres.sh
# Script description:	Makes ICs on fv3 stand-alone regional grid 
#                       using FV3GFS initial conditions.
# Script history log:
#   1) 2016-09-30       Fanglin Yang
#   2) 2017-02-08	Fanglin Yang and George Gayno
#			Use the new CHGRES George Gayno developed.
#   3) 2019-05-02	Ben Blake
#			Created exfv3cam_sar_chgres.sh script
#			from global_chgres_driver.sh
############################################################################
set -ax

# gtype = regional
echo "creating standalone regional BCs"
export ntiles=1
export TILE_NUM=7

#
# set the links to use the 4 halo grid and orog files
# these are necessary for creating the boundary data
#
ln -sf $FIXsar/${CASE}_grid.tile7.halo4.nc $FIXsar/${CASE}_grid.tile7.nc 
ln -sf $FIXsar/${CASE}_oro_data.tile7.halo4.nc $FIXsar/${CASE}_oro_data.tile7.nc 
ln -sf $FIXsar/${CASE}.vegetation_greenness.tile7.halo4.nc $FIXsar/${CASE}.vegetation_greenness.tile7.nc
ln -sf $FIXsar/${CASE}.soil_type.tile7.halo4.nc $FIXsar/${CASE}.soil_type.tile7.nc
ln -sf $FIXsar/${CASE}.slope_type.tile7.halo4.nc $FIXsar/${CASE}.slope_type.tile7.nc
ln -sf $FIXsar/${CASE}.substrate_temperature.tile7.halo4.nc $FIXsar/${CASE}.substrate_temperature.tile7.nc
ln -sf $FIXsar/${CASE}.facsf.tile7.halo4.nc $FIXsar/${CASE}.facsf.tile7.nc
ln -sf $FIXsar/${CASE}.maximum_snow_albedo.tile7.halo4.nc $FIXsar/${CASE}.maximum_snow_albedo.tile7.nc
ln -sf $FIXsar/${CASE}.snowfree_albedo.tile7.halo4.nc $FIXsar/${CASE}.snowfree_albedo.tile7.nc
ln -sf $FIXsar/${CASE}.vegetation_type.tile7.halo4.nc $FIXsar/${CASE}.vegetation_type.tile7.nc

#
# create namelist and run chgres cube
#
cp ${CHGRESEXEC} .

# NHRS = lentgh of free forecast
# NHRSda = length of DA cyce forecast (always 1-h)
if [ $tmmark = tm00 ]; then
  hour=3
  end_hour=$NHRS
  hour_inc=3
else
  hour=0
  end_hour=$NHRSda
  hour_inc=1
fi

while (test "$hour" -le "$end_hour")
  do
  if [ $hour -lt 10 ]; then
    hour_name='00'$hour
  elif [ $hour -lt 100 ]; then
    hour_name='0'$hour
  else
    hour_name=$hour
  fi


rm poe.${hour_name}

mkdir -p ${hour_name}
cd ${hour_name}

rm ./*

echo "&config" > fort.41
echo ' mosaic_file_target_grid="_FIXsar_/_CASE__mosaic.nc"' >> fort.41
echo ' fix_dir_target_grid="_FIXsar_"' >> fort.41
echo ' orog_dir_target_grid="_FIXsar_"' >> fort.41
echo ' orog_files_target_grid="_CASE__oro_data.tile7.halo4.nc"' >> fort.41 
echo ' vcoord_file_target_grid="_FIXam_/global_hyblev.l_LEVS_.txt"' >> fort.41 
echo ' mosaic_file_input_grid="NULL"' >> fort.41
echo ' orog_dir_input_grid="NULL"' >> fort.41
echo ' orog_files_input_grid="NULL"' >> fort.41
echo ' data_dir_input_grid="_INIDIR_"' >> fort.41
echo ' atm_files_input_grid="gfs.t_cyc_z.atmf_hour_name_.nemsio"' >> fort.41
echo ' sfc_files_input_grid="gfs.t_cyc_z.sfcanl.nemsio"' >> fort.41
echo " cycle_mon=$month" >> fort.41
echo " cycle_day=$day" >> fort.41
echo " cycle_hour=$cyc" >> fort.41
echo " convert_atm=.true." >> fort.41
echo " convert_sfc=.false." >> fort.41
echo " convert_nst=.false." >> fort.41
echo ' input_type="gaussian"' >> fort.41
echo ' tracers="sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"' >> fort.41
echo ' tracers_input="spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"' >> fort.41
echo " regional=${REGIONAL}" >> fort.41
echo " halo_bndy=${HALO}" >> fort.41
echo "/" >>  fort.41


cat fort.41 | sed s:_FIXsar_:${FIXsar}:g \
            | sed s:_CASE_:${CASE}:g \
            | sed s:_FIXam_:${FIXam}:g  \
            | sed s:_LEVS_:${LEVS}:g \
            | sed s:_INIDIR_:${INIDIR}:g \
            | sed s:_cyc_:${cyc}:g \
            | sed s:_hour_name_:${hour_name}:g > fort.41.new

mv fort.41 fort.41.old
mv fort.41.new fort.41
            

cd ../

echo "cd ${hour_name}" >  poe.${hour_name}

echo "cp ../regional_chgres_cube.x ." >> poe.${hour_name}

echo "${APRUNC} ./regional_chgres_cube.x" >> poe.${hour_name}

echo "mv gfs.bndy.nc $INPdir/gfs_bndy.tile7.${hour_name}.nc " >> poe.${hour_name}

chmod u+x poe.${hour_name}
./poe.${hour_name} &

hour=`expr $hour + $hour_inc`

#
# move output files to save directory
#
#   mv gfs.bndy.nc $INPdir/gfs_bndy.tile7.${hour_name}.nc

done

wait

echo " All poe scripts completed - normal finish "


exit 0
