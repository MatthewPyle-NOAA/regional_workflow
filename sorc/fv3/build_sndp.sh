#! /bin/sh


module purge >& /dev/null

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load -a ../modulefiles_fv3/wcoss_cray/v8.0.0-cray-intel
module list

cd ./regional_sndp.fd

make delete

make

cd ../

