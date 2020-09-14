
BASE=${BASE:-"garbage"}

if [ $BASE == "garbage" ]
then
echo " "
echo "NEED TO RUN THIS SCRIPT FROM MAIN build_hiresw.sh SCRIPT"
echo " "
exit
fi

set -x


##############################

module load iobuf/2.0.7

cd ${BASE}/hiresw_wrfbufr.fd
make clean
make

module unload iobuf/2.0.7


cd ${BASE}


##############################
