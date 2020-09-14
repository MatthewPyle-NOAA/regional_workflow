
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

export WRF_NMM_CORE=0
export WRF_EM_CORE=1

TARGDIR=../../exec

############################

cd ${BASE}/hiresw_wrfarwfcst.fd

./clean -a
cp configure.wrf_wcoss configure.wrf

# build model
./compile  em_real 

# build real
./compile  em_real 


# switch this to install script
# cp ./main/real.exe $TARGDIR/hiresw_wrfarwfcst_init
# cp ./main/wrf.exe  $TARGDIR/hiresw_wrfarwfcst


cd ${BASE}


##############################
