#!/bin/sh --login
set -x -u
date

. ${HOMEfv3}/rocoto/machine-setup.sh
export machine=${target}

if [ "$machine" = "wcoss_dell_p3" ] ; then
  . /usrx/local/prod/lmod/lmod/init/sh
elif [ "$machine" = "wcoss_cray" ] ; then
  . /opt/modules/default/init/sh
fi

job=${job:-${jobid}}

module use ${HOMEfv3}/sorc/modulefiles_fv3/
jobpre=$(echo ${job} | cut -c1-17)

echo job is $job

echo ${job} | grep 'forecast'
err=$?

echo here with err $err

if [ $err -eq 0 ]; then
  module load run_fv3model
else
  module load run_fv3other
fi
module list


# export WGRIB2=${HOMEfv3}/exec/fv3/hireswfv3_wgrib2

# export WGRIB2=/gpfs/hps/nco/ops/nwtest/lib/wgrib2/v2.0.8/NCEPLIBS-wgrib2/build/wgrib2/wgrib2
export WGRIB2=/gpfs/hps/nco/ops/nwtest/lib/wgrib2/v2.0.8/intel/bin/wgrib2

export COMROOT=${MYCOMROOT}
export RUN_ENVIR=dev

export NWROOT=/gpfs/hps/nco/ops/nwprod

export HOMEobsproc_shared_bufr_cword=$NWROOT/obsproc_shared/bufr_cword.v1.0.0
export CCPP="true"
export OMP_NUM_THREAD=1

echo here in rocoto_pre_job with WGRIB2 as $WGRIB2

exec "$@"
