#! /bin/sh


module purge >& /dev/null

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles_fv3/wcoss_cray/hiresw_fv3_module


module list

echo COMPILER $COMPILER

cd ./hireswfv3_fv3snowbucket.fd

make clean

make hireswfv3_fv3bucket

cd ../

