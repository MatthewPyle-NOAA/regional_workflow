
set -x

BASE=`pwd`

module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ./hiresw_module


##############################

cd ${BASE}/regional_smartprecip.fd
make clean
make


cd ${BASE}


##############################
