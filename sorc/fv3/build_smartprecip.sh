
set -x

BASE=`pwd`

module purge

module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load -a ../modulefiles_fv3/wcoss_cray/v8.0.0-cray-intel



##############################

cd ${BASE}/regional_smartprecip.fd
make clean
make


make clean

cd ${BASE}


##############################
