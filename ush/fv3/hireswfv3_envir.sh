set -x

export RUN_ENVIR=dev
export envir=para

export NET=hiresw
export RUN=hiresw
export model=fv3sar	# fv3sar (cold start) or fv3sar_da (with DA)
export CCPP=false
export CCPP_SUITE=FV3_GFS_2017_gfdlmp_regional

if [ ${machine} = "wcoss_dell_p3" ]
then

export COMINgdas=${COMINgdas:-/gpfs/dell1/nco/ops/com/gfs/prod}
export GBCOMINgfs=${GBCOMINgfs:-/gpfs/dell1/nco/ops/com/gfs/prod}
export COMINgfs=${COMINgfs:-/gpfs/dell1/nco/ops/com/gfs/prod}

export PTMP=${PTMP:-/gpfs/dell1/ptmp/${USER}}
export STMP=${STMP:-/gpfs/dell1/stmp/${USER}}
export rzdmuser=bblake

export NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate
export NHOUR=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/nhour
export HOMEfv3=${HOMEfv3:-/gpfs/dell2/emc/modeling/noscrub/$USER/regional_workflow}

elif [ ${machine} = "wcoss_cray" ]
then


export NWROOT=/gpfs/hps/nco/ops/nwprod
export SENDECF=NO
export hiresw_ver=v8.0.0
export COMINgdas=${COMINgdas:-/gpfs/dell1/nco/ops/com/gfs/prod}
export GBCOMINgfs=${GBCOMINgfs:-/gpfs/dell1/nco/ops/com/gfs/prod}
export COMINgfs=${COMINgfs:-/gpfs/dell1/nco/ops/com/gfs/prod}

export MYCOMROOT=${MYCOMROOT:-/gpfs/hps3/ptmp/${USER}/com}
export DATAROOT=${DATAROOT:-/gpfs/hps3/stmp/${USER}/tmpnwprd}
export rzdmuser=mpyle

export HOMEfv3=${HOMEfv3:-/gpfs/hps3/emc/meso/noscrub/$USER/hiresw.${hiresw_ver}}
# export NDATE=/gpfs/hps/nco/ops/nwprod/prod_util.v1.1.0/exec/ndate
# export NHOUR=/gpfs/hps/nco/ops/nwprod/prod_util.v1.1.0/exec/nhour


else

echo UNDEFINED ENVIRONMENT
exit 77

fi


export CRES=768          #-- FV3 equivalent to 13-km global resolution
export CASE=C${CRES}
export gtype=regional    # grid type = uniform, stretch, nest, or regional

export PARMfv3=$HOMEfv3/parm/fv3
export EXECfv3=$HOMEfv3/exec/fv3
export USHfv3=$HOMEfv3/ush/fv3

export FIXfv3=$HOMEfv3/fix/fv3
export FIXsar=$FIXfv3/fix_sar/${NEST}
export FIXam=$FIXfv3/fix_am
export FIXco2=$FIXam/fix_co2_proj

export COMROOT=${MYCOMROOT}
export GESROOT=${MYCOMROOT}/../nwges

export NHRS=60          #-- Forecast length for free fcst

export jlogfile=${DATAROOT}/../jlogfile.${RUN}.jlog
