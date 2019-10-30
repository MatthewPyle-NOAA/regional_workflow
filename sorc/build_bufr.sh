#! /bin/sh


module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ./hiresw_module

module list

cd ./regional_bufr.fd

# make clean

rm PROF_FV3SAR_NET.o
make


cd ../regional_sndp.fd

make
