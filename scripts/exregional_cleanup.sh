#!/bin/sh
#############################################################################
# Script name:		exfv3cam_cleanup.sh
# Script description:	Scrub old files and directories
# Script history log:
#   1) 2018-04-09	Ben Blake
#			new script
#############################################################################
set -x

# Remove temporary working directories
cd ${STMP}
if [ $RUN = fv3sar -o $RUN = hiresw ]; then
  cd tmpnwprd
elif [ $RUN = fv3nest ]; then
  cd tmpnwprd_nest
fi


# jobtypes="bufrpost forecast_tm00 make_bc make_ic posteven postodd smartinit smartinitb"
jobtypes="bufrpost make_bc make_ic postodd smartinit smartinitb"

for job in $jobtypes
do
rm -rf regional_${job}_${dom}_${CDATE}
done

if [ $dom = ak -o $dom = conus ]
then
 jobtypesb="prdgeneven prdgenodd"
else
 jobtypesb="prdgen"
fi

for job in $jobtypesb
do
rm -rf regional_${job}_${dom}_${CDATE}
done


exit
