
set -x

BASE=`pwd`

module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles_fv3/build_v8.0.0-cray-intel



##############################

cd ${BASE}/hireswfv3_smartprecip.fd
make clean
make


make clean

cd ${BASE}


##############################
