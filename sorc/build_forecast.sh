#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

if [ $target = hera ]; then target=hera.intel ; fi

export CCPP=true

cd regional_forecast.fd/
FV3=$( pwd -P )/FV3
CCPP=${CCPP:-"false"}
cd tests/

echo target is $target

if [ $CCPP  = true ] || [ $CCPP = TRUE ] ; then
#orig  ./compile.sh "$FV3" "$target" "NCEP64LEV=Y HYDRO=N 32BIT=Y REPRO=Y CCPP=Y STATIC=Y SUITES=FV3_GFS_v15_thompson_mynn,FV3_GFS_v15_gf_thompson,FV3_GSD_v0,FV3_GSD_noah" 1
#thisoptionworkedbetterin_rt  
./compile.sh "$FV3" "$target" "CCPP=Y STATIC=Y SUITES=FV3_GFS_v15_thompson_mynn,FV3_GFS_v15_gf_thompson,FV3_GSD_v0,FV3_GSD_noah" 1
else
  ./compile.sh "$FV3" "$target" "NCEP64LEV=Y HYDRO=N 32BIT=Y" 1
fi
mv -f fv3_1.exe ../NEMS/exe/NEMS.x
