#! /bin/sh


module purge >& /dev/null

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

# module load -a ../modulefiles_fv3/hiresw_fv3_module
module load -a ../modulefiles_fv3/v8.0.0-cray-intel



module list

cd ./hireswfv3_bucket.fd

make clean

make 

cd ../

