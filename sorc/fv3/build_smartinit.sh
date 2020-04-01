set -x

BASE=`pwd`

module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

# module load -a ./hiresw_module
module load -a ../modulefiles/wcoss_cray/v8.0.0-cray-intel



##############################

module load iobuf/2.0.7
cd ${BASE}/regional_smartinit.fd
make clean
make

module unload iobuf/2.0.7

make clean

cd ${BASE}


##############################
