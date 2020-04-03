#!/bin/sh
################################################################3
#
#  This script will tar up all the data for a given forecast cycle for
#  the directory specified by the first
#  argument ($1) and place the tar file on the HPSS server,
#  under /hpssprod/runhistory.  The tar file is put in the directory
#  appropriate for data valid for the day specified as the second 
#  command line argument ($2).
#
#  This script breaks up the nam data directory into five separate
#  tar files ( that is, five tar files per cycle ).
#  The data files are broken up as proposed by Benjamin.Blake, EMC/MMB.
#
#  Nam restart files are also copied from /nwges/prod/nam.YYYMMDD to
#  the appropriate /hsmprod/runhistory directory.
#
#  Usage: rhist_savefv3sar.sh Directory Date(YYYYMMDDHH format)
#
#  Where: Directory  = Directory to be tarred.
#         Date(YYYYMMDDHH format) = Day that the tar file should be saved under.
#
################################################################3
set -x


if [ $# -ne 2 ]
then
  echo "Usage: rhist_savefv3sar.sh Directory Date(YYYYMMDDHH format) "
  exit 1
fi 

#
#   Get directory to be tarred from the first command line argument,
#   and check to make sure that the directory exists.
#

dir=$1
if [ ! -d $dir ]
then
  echo "rhist_savefv3sar.sh:  Directory $dir does not exist."
  exit 2
fi 

#
#   Determine the directory where the tar file will be stored
#   and make sure that it exists in HPSS
#

year=`echo $2 | cut -c 1-4`
yearmo=`echo $2 | cut -c 1-6`
yrmoday=`echo $2 | cut -c 1-8`
rhcyc=`echo $2 | cut -c 9-10`
rhcycle=t${rhcyc}z

echo USER is $USER

hpssdir0=/NCEPDEV/emc-meso/2year/${USER}/fv3sar/rh${year}/${yearmo}/$yrmoday
hpssdir1=/NCEPDEV/emc-meso/2year/${USER}/fv3sar/rh${year}/${yearmo}/$yrmoday
hpssdir2=/NCEPDEV/emc-meso/2year/${USER}/fv3sar/rh${year}/${yearmo}/$yrmoday

hsi "mkdir -p $hpssdir0"

#
#   Get a listing of all files in the directory to be tarred
#   and break the file list up into groups of files.
#   Each list of files names the contents of its associated tar file.
#   Then cd to the directory to be tarred.
# 

cd $DATA

hrs_save="00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60"

if [ -e ${dom} ]
then
rm ./${dom}
rm ./therest
fi

for hr in $hrs_save
do


if [ ${dom} = "conus" ]
then
ls -1 $dir | grep ${rhcycle} | grep f${hr} | awk '
            /conus.grib2/ { print "./"$0 >> "conus" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "pr" ]
then
ls -1 $dir | grep ${rhcycle} |  grep f${hr} | awk '
            /pr.grib2/ { print "./"$0 >> "pr" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "ak" ]
then
ls -1 $dir | grep ${rhcycle} |  grep f${hr} | awk '
            /ak.grib2/ { print "./"$0 >> "ak" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "hi" ]
then
ls -1 $dir | grep ${rhcycle} |  grep f${hr} | awk '
            /hi.grib2/ { print "./"$0 >> "hi" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "guam" ]
then
ls -1 $dir | grep ${rhcycle} |  grep f${hr} | awk '
            /guam.grib2/ { print "./"$0 >> "guam" ; next }
            { print "./"$0 >> "therest" ; next } '
fi

done

if [ ${dom} = "conus" ]
then
ls -1 $dir | grep ${rhcycle} | awk '
            /conusfv3.class1.bufr/ { print "./"$0 >> "conus" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "pr" ]
then
ls -1 $dir | grep ${rhcycle} | awk '
            /prfv3.class1.bufr/ { print "./"$0 >> "pr" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "ak" ]
then
ls -1 $dir | grep ${rhcycle} |  awk '
            /akfv3.class1.bufr/ { print "./"$0 >> "ak" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "hi" ]
then
ls -1 $dir | grep ${rhcycle} | awk '
            /hifv3.class1.bufr/ { print "./"$0 >> "hi" ; next }
            { print "./"$0 >> "therest" ; next } '
elif [ ${dom} = "guam" ]
then
ls -1 $dir | grep ${rhcycle} | awk '
            /guamfv3.class1.bufr/ { print "./"$0 >> "guam" ; next }
            { print "./"$0 >> "therest" ; next } '
fi


cd $dir

#  Now create a tar file for each group of files

for file in ${dom}
do

   # 
   #   Pick 1year, 2year, or permanent archive.
   #
   case $file in
      conus|ak|hi|pr|guam)  hpssdir=$hpssdir0
              rhistdir=$rhistdir0;;
   esac

   #
   #   Generate the name of the tarfile, which should be the same
   #   as the absolute path name of the directory being
   #   tarred, except that "/" are replaced with "_".
   #

   tarfile=`echo $PWD | cut -c 2- | tr "/" "_"`
   tarfile=${tarfile}${rhcyc}.${file}.tar
   tarfile=fv3sar.${yrmoday}${rhcyc}.${file}.tar

   #
   #   Check if the tarfile index exists.  If it does, assume that
   #   the data for the corresponding directory has already been
   #   tarred and saved.
   #

   hsi "ls -l ${hpssdir}/${tarfile}.idx"
   tar_file_exists=$?
   if [ $tar_file_exists -eq 0 ]
   then
     echo "File $tarfile already saved."
     continue
   fi

   date
   htar -P -cvf ${hpssdir}/$tarfile -L ${DATA}/$file
   err=$?
   if [ $err -ne 0 ]
   then
     echo "rhist_savefv3sar.sh:  File $tarfile was not successfully created."
     exit 3
   fi
   date
 
   #
   #   Read the tarfile and save a list of files that are in the tar file.
   #
 
   htar -tvf $hpssdir/$tarfile
   err=$?
   if [ $err -ne 0 ]
   then
     echo "rhist_savefv3sar.sh:  Tar file $tarfile was not successfully read to"
     echo "             generate a list of the files."
     exit 4
   fi
 
   #
   #  Restrict tar file, if it contains restricted data.
   #
    ${USHdir}/rhist_restrict.sh ${hpssdir}/$tarfile
 
   rm ${DATA}/$file
   
done

cd $DATA

echo done with FV3 archive
