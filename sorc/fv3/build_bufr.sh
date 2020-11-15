#! /bin/sh


source ./machine-setup.sh > /dev/null 2>&1

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles_fv3/build_v8.0.0-cray-intel


module list

cd ./hireswfv3_bufr.fd

make clean
make

make clean

cd ../

