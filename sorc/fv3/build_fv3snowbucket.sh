#! /bin/sh


module purge >& /dev/null

# module load ncep
# module load craype-sandybridge
# module use -a /opt/cray/modulefiles

source /apps/prod/lmodules/startLmod

module use -a ../modulefiles_fv3/
module load build_v8.0.0-cray-intel

module list

echo COMPILER $COMPILER

cd ./hireswfv3_fv3snowbucket.fd

make clean

make hireswfv3_fv3bucket

cd ../

