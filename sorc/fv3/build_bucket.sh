#! /bin/sh


module purge >& /dev/null

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles_fv3/wcoss_cray/hiresw_fv3_module


module list

cd ./hireswfv3_bucket.fd

make clean

make 

cd ../

