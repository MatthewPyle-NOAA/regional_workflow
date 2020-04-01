#!/bin/sh
set -xeu

LINK="cp -rp"
pwd=$(pwd -P)

FIX_DIR="/gpfs/dell2/emc/modeling/noscrub/emc.campara/fix_hrefv3_fv3cam"

mkdir -p ${pwd}/../../fix
cd ${pwd}/../../fix                || exit 8
for dir in fix_arw  ; do
    [[ -d $dir ]] && rm -rf $dir
done

${LINK} $FIX_DIR/fix_arw .
${LINK} $FIX_DIR/wrflibs  ${pwd}/hiresw_wrfbufr.fd/

exit
