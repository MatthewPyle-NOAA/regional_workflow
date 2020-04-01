#! /bin/sh


BASE=${BASE:-"garbage"}

if [ $BASE == "garbage" ]
then
echo " "
echo "NEED TO RUN THIS SCRIPT FROM MAIN build_hiresw.sh SCRIPT"
echo " "
exit
fi



set +x

############################

cd ${BASE}/hiresw_wps.fd


./clean -a

cp configure.wps_serial_nowrf configure.wps

./compile ungrib >& build_wps_ungrib.log

# move this copy to install script
## cp ungrib.exe ../../exec/hiresw_wps_ungrib

# ./compile geogrid >& build_geogrid.log

cp configure.wps_serial configure.wps

./compile metgrid >& build_wps_metgrid.log

# move this copy to install script
## cp metgrid.exe ../../exec/hiresw_wps_metgrid


cd ${BASE}
