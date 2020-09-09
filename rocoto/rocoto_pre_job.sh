#!/bin/sh --login
set -x -u -e
date

. ${HOMEfv3}/rocoto/machine-setup.sh
export machine=${target}

if [ "$machine" = "wcoss_dell_p3" ] ; then
  . /usrx/local/prod/lmod/lmod/init/sh
elif [ "$machine" = "wcoss_cray" ] ; then
  . /opt/modules/default/init/sh
fi

module use ${HOMEfv3}/sorc/modulefiles_fv3/
jobpre=$(echo ${job} | cut -c1-17)
if [ "${jobpre}" = "forecast" ]; then
  module load fv3
  echo loaded fv3
else
  module load regional
  echo loaded regional
fi
module list


export WGRIB2=${HOMEfv3}/exec/fv3/hireswfv3_wgrib2

export COMROOT=${MYCOMROOT}
export RUN_ENVIR=dev
export GESROOT=${MYGESROOT}

echo here in rocoto_pre_job with WGRIB2 as $WGRIB2

exec "$@"
