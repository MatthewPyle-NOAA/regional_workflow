#! /bin/sh


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


cd ${BASE}/hiresw_bucket.fd
make


cd ${BASE}

##############################
