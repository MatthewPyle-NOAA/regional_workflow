set -x

BASE=`pwd`

module purge

# module load ncep
# module load craype-sandybridge
# module use -a /opt/cray/modulefiles

source /apps/prod/lmodules/startLmod


module use -a ../modulefiles_fv3
module load build_v8.0.0-cray-intel



##############################

# module load iobuf/2.0.7
cd ${BASE}/hireswfv3_smartinit.fd
make clean
make

# module unload iobuf/2.0.7

make clean

cd ${BASE}


##############################
