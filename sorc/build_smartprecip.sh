
set -x

BASE=`pwd`

module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

# module load -a ./hiresw_module

module load -a ../modulefiles/wcoss_cray/v8.0.0-cray-intel



##############################

cd ${BASE}/regional_smartprecip.fd
make clean
make


cd ${BASE}


##############################
