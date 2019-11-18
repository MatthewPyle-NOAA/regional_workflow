#! /bin/sh


module purge >& /dev/null

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles/wcoss_cray/hiresw_fv3_module

module list

cd ./regional_bufr.fd

make clean

make

cd ../

