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


jobtypes="bufrpost forecast make_bc make_ic posteven postodd smartinit smartinitb"

for job in $jobtypes
do
echo cyc is $cyc
echo envir is $envir

rm -rf hiresw.${job}_${NEST}_fv3_${cyc}_${envir}
done

if [ $NEST = ak -o $NEST = conus ]
then
 jobtypesb="prdgeneven prdgenodd"
else
 jobtypesb="prdgen"
fi

for job in $jobtypesb
do
rm -rf hiresw.${job}_${NEST}_fv3_${cyc}_${envir}
done


exit
