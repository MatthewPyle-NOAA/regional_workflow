#! /usr/bin/env bash
set -eux

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi


## how modify this for wcoss2??

cd hireswfv3_forecast.fd/
FV3=$( pwd -P )/FV3
CCPP=${CCPP:-"true"}
cd tests/
if [ $CCPP  = true ] || [ $CCPP = TRUE ] ; then
  ./compile_gmake.sh "$FV3" wcoss_phase2 "CCPP=Y SUITES=FV3_GFS_2017_gfdlmp_regional,FV3_GFS_2017_gfdlmp_regional_c768 32BIT=Y" href YES YES
else
  ./compile_gmake.sh "$FV3" wcoss_phase2 "NCEP64LEV=Y HYDRO=N 32BIT=Y" 1
fi

mv -f fv3_href.exe ../NEMS/exe/NEMS.x
