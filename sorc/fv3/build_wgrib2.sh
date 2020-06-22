#! /bin/sh


source ./machine-setup.sh > /dev/null 2>&1

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles_fv3/wcoss_cray/v8.0.0-cray-intel


# module load gcc/4.9.2

module list

cd ./hireswfv3_wgrib2.cd/grib2

make deep-clean
make
make lib

# make clean

# module unload gcc

cd ../
