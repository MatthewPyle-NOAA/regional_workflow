#!/bin/sh
set -xeu

LINK="cp -rp"
pwd=$(pwd -P)

FIX_DIR="/gpfs/dell2/emc/modeling/noscrub/emc.campara/fix_hrefv3_fv3cam"

mkdir -p ${pwd}/../../fix/arw
cd ${pwd}/../../fix/arw                || exit 8

${LINK} $FIX_DIR/fix_arw/* .
${LINK} $FIX_DIR/wrflibs  ${pwd}/hiresw_wrfbufr.fd/

exit
