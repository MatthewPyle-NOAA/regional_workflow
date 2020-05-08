#!/bin/sh
set -xeu

source ./machine-setup.sh > /dev/null 2>&1

LINK="cp -rp"

pwd=$(pwd -P)

if [[ ${target} == "wcoss_dell_p3" || ${target} == "wcoss" ||  ${target} == "wcoss_cray" ]]; then
    FIX_DIR="/gpfs/dell2/emc/modeling/noscrub/emc.campara/fix_hrefv3_fv3cam"
elif [ ${target} == "hera" ]; then
    FIX_DIR="/scratch2/NCEPDEV/fv3-cam/emc.campara/fix_fv3cam"
elif [ ${target} == "jet" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix"
else
    echo "Unknown site " ${target}
    exit 1
fi

mkdir -p ${pwd}/../../fix/fv3
cd ${pwd}/../../fix/fv3                ||exit 8
for dir in fix_am fix_sar ; do
    [[ -d $dir ]] && rm -rf $dir
done

${LINK} $FIX_DIR/fix_am .
${LINK} $FIX_DIR/fix_sar .
${LINK} $FIX_DIR/wrflibs  ${pwd}/hireswfv3_bufr.fd/

exit