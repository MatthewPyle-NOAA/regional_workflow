#! /bin/sh


source ./machine-setup.sh > /dev/null 2>&1

module purge

# module load ncep
# module load craype-sandybridge
# module use -a /opt/cray/modulefiles

source /apps/prod/lmodules/startLmod


module use -a ../modulefiles_fv3/
module load build_v8.0.0-cray-intel


module list

cd ./hireswfv3_bufr.fd

make clean
make

make clean

cd ../

