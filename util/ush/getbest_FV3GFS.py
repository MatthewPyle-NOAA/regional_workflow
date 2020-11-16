"""
 Name:  getbest_FV3GFS.py            Author:  Jacob Carley 

 Abstract:
 This Python script returns the closest valid EnKF forecast output in the form
 of complete file paths in a user defined outputfile.

 Usage: python getbest_FV3GFS.py -d [COM_EnKF] -v [valid time in YYYYMMDDHH] -t [hours] -r [resolution]
                               -o [output file] -m [ens mean?] -s [starting fhr] --retro=yes/no --exact=yes/no
                               --minsize=x --o3fname=filename --filetype=atm/sfc --4d=[start,stop,inchrs] 
                               --gfs_nemsio=yes/no --gfs_netcdf=yes/no -h

 -d [COM] = Directory where output resides, e.g. /com/gfs/prod/ (omit the .YYYYMMDDHH)
 -v [vtime] = valid time in YYYYMMDDHH (i.e. GSI analysis time)
 -t [hours] = Look back time, in hours from vtime, to start search for best set of GFS inputs
 -o [output file] = Name of the outputfile
 -s [starting fhr] = Minimum limit on EnKF member forecast length (i.e. return no matches less than or equal this number)
 --retro=yes/no: whether or not this is a retro run (default is no). If yes it grabs the INPUT envar and searches only that directory
 --exact=yes/np: whether the retrieved EnKF members must be valid exactly at vtime (Default is NO)
 --gfs_nemsio=yes/no : Whether to look for input EnKF files from the GFS in nemsio format (Default is NO).
 --gfs_netcdf=yes/no : Whether to look for input EnKF files from the GFS in netcdf format (Default is NO).
 -h = Prints this output

 History: 2015-09-04    Carley       Initial implementation
          2015-09-18    Carley       Updated for retro runs, exact dates, min ensemble size, and o3 file linking
          2015-09-18    Carley       Updated for 4DEnVar, where we want all EnKF forecasts to be from the same cycle
          2017-01-03    Carley       Updated for new gfs file name conventions
          2019-10-30    Pyle         Simplified for getting 6 h old GFS for HREF FV3SAR
          2020-09-23    Pyle         Adapted for GFSv16 netcdf replacing nemsio output
"""

import sys,os,getopt,time,errno
from datetime import datetime,timedelta

def usage():
    print ("Usage: python %s -d [COM_EnKF] -v [valid time in YYYYMMDDHH] -t [hours] -r [resolution] \n" 
           "\t\t\t\t-o [output file] -m [ens mean?] -s [starting fhr] --retro=yes/no --exact=yes/no \n"
           "\t\t\t\t--minsize=x --o3fname=filename --filetype=atm/sfc --4d=[start,stop,inchrs] -h") % (sys.argv[0])
    print
    print " -d [COM_EnKF] = Directory where EnKF output resides, e.g. /com/gfs/prod/enkf (omit the .YYYYMMDDHH)"
    print " -v [vtime] = valid time in YYYYMMDDHH (i.e. GSI analysis time)"
    print " -t [hours] = Look back time, in hours from vtime, to start search for best set of EnKF members"
    print " -o [output file] = Name of the outputfile"
    print " --exact=yes/no: whether the retrieved EnKF members must be valid exactly at vtime (default is NO)"
    print " --gfs_nemsio=yes/no : Whether to look for input EnKF files from the GFS in nemsio format (Default is NO)."
    print " --gfs_netcdf=yes/no : Whether to look for input EnKF files from the GFS in netcdf format (Default is NO)."
    print " -h = Prints this output"

def is_non_zero_file(fpath):
    return True if os.path.isfile(fpath) and os.path.getsize(fpath) > 0 else False

def force_symlink(f1,f2):
    try:
        os.symlink(f1,f2)
    except OSError, e:
        if e.errno == errno.EEXIST:
            os.remove(f2)
            os.symlink(f1,f2)

def write_filelist(fname,comgfs,fsave,svdate,retro,path,suf,o3fname,filetype,getmean,gfs_nemsio,gfs_netcdf):
    svcdate=svdate.strftime('%Y%m%d%H')
    svPDY=svdate.strftime('%Y%m%d')
    svCYC=svdate.strftime('%H')
    if retro:
        if 'INPUT' in os.environ:
            path=os.environ['INPUT']
        else:
            sys.exit("Unable to locate INPUT in your environment!\n" \
                     "Therefore cannot find where EnKF members reside for Retro. Exit.")
    else:
        path=comgfs+'.'+svPDY+'/'+svCYC+'/atmos'
        print 'set path here to : ', path
    f=open(fname,'w')
    havefile=True   
    n=0
    while havefile: # first while loop code
        if gfs_nemsio:
            n=n+1
            en=path+'/gfs.t'+svCYC+'z.'+filetype+'f'+str(fsave).zfill(3)+'.nemsio'
            justfile='gfs.t'+svCYC+'z.'+filetype+'f'+str(fsave).zfill(3)+'.nemsio'
        if gfs_netcdf:
            n=n+1
            en=path+'/gfs.t'+svCYC+'z.'+filetype+'f'+str(fsave).zfill(3)+'.nc'
            justfile='gfs.t'+svCYC+'z.'+filetype+'f'+str(fsave).zfill(3)+'.nc'
        if is_non_zero_file(en):
            f.write(path+'\n')
            f.write(justfile+'\n')
            print 'Will copy file %s from GFS ' %(en)
        else:
            havefile=False
        havefile=False



def checkmembers(thispath,fhr,thiscdate,thissuf,gfs_nemsio,gfs_netcdf):
    n=0
    havefile=True
    fhrs=str(fhr).zfill(2)
    while havefile:
        n=n+1
        mem=str(n).zfill(3)
        if gfs_nemsio:
            CYC=thiscdate[8:10] 
            f=thispath+'/mem'+mem+'/gdas.t'+CYC+'z.atmf'+str(fhr).zfill(3)+'s.nemsio'
        elif gfs_netcdf:
            CYC=thiscdate[8:10] 
            f=thispath+'/mem'+mem+'/gdas.t'+CYC+'z.atmf'+str(fhr).zfill(3)+'s.nc'
        else:
            f=thispath+'/sfg_'+thiscdate+'_fhr'+str(fhr).zfill(2)+'s_mem'+mem+thissuf
        if not is_non_zero_file(f):
            havefile=False
            n=n-1   # Remove the erroneously added member
    return n

def checkfile(thispath,fhr,thiscdate,thissuf,gfs_nemsio,gfs_netcdf):
    n=0
    havefile=True
    fhrs=str(fhr).zfill(2)
    while havefile:
        n=n+1
        if gfs_nemsio:
            CYC=thiscdate[8:10] 
            f=thispath+'/gfs.t'+CYC+'z.atmf'+str(fhr).zfill(3)+'s.nemsio'
        elif gfs_netcdf:
            CYC=thiscdate[8:10] 
            f=thispath+'/gfs.t'+CYC+'z.atmf'+str(fhr).zfill(3)+'s.nc'
        else:
            f=thispath+'/sfg_'+thiscdate+'_fhr'+str(fhr).zfill(2)+'s_mem'+mem+thissuf
        if not is_non_zero_file(f):
            havefile=False
            n=n-1   # Remove the erroneously added member
    return n

def main():

    try:
        opts, args = getopt.getopt(sys.argv[1:], "d:v:t:r:o:m:s:h",["retro=","exact=","minsize=","o3fname=","filetype=","4d=","gfs_nemsio=","gfs_netcdf="])
    except getopt.GetoptError as err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit()

    # Set the defaults
    svdate=None
    cdate=datetime.today().strftime('%Y%m%d%H')#Use current YYYYMMDDHH as default
    comgfs='/gpfs/dell1/nco/ops/com/gfs/prod/'
    tm=24
    suf=""      #Use full resolution
    fname='filelist'
    getmean=True
    sfhr=0
    retro=False
    exact=False
    minsize=1
    o3fname=None
    fourd=None
    gfs_nemsio=False
    gfs_netcdf=False

    for o, a in opts:
        if o == "-d":
            comgfs=str(a)
        elif o == "-v":
            cdate=str(a)
        elif o == "-t":
            tm=int(a)
        elif o == "-r":
            if a.strip().upper()=='L':
                suf="_t254"
            elif a.strip().upper()=='F':
                suf=""
        elif o == "-o":
            fname=str(a)
        elif o == "-m":
            if str(a).upper() == 'NO' or str(a).upper() == 'N':
                getmean = False
                minsize=minsize-1
            elif str(a).upper() == 'YES' or str(a).upper() == 'Y':
                getmean = True
            else:
                usage()
                sys.exit("Invalid choice for returning ensemble mean. yes or no only..")
        elif o == "-s":
            sfhr=int(a)
        elif o == "--retro":
            if str(a).upper() == 'NO' or str(a).upper() == 'N':
                retro = False
            elif str(a).upper() == 'YES' or str(a).upper() == 'Y':
                retro = True
            else:
                usage()
                sys.exit("Invalid choice for --retro. yes or no only..")
        elif o == "--exact":
            if str(a).upper() == 'NO' or str(a).upper() == 'N':
                exact = False
            elif str(a).upper() == 'YES' or str(a).upper() == 'Y':
                exact = True
            else:
                usage()
                sys.exit("Invalid choice for --exact. yes or no only..")
        elif o == "--minsize":
            minsize=int(a)
        elif o == "--o3fname":
            o3fname=str(a)
        elif o == "--filetype":
            filetype=str(a)
        elif o == "--4d":
            fourd=str(a).split(',')
            fourd=[q.replace('[','') for q in fourd]
            fourd=[q.replace(']','') for q in fourd]
        elif o == "--gfs_nemsio":
            if str(a).upper() == 'NO' or str(a).upper() == 'N':
               gfs_nemsio = False
            elif str(a).upper() == 'YES' or str(a).upper() == 'Y':
               gfs_nemsio = True
            else:
                usage()
                sys.exit("Invalid choice for --gfs_nemsio. yes or no only..")
        elif o == "--gfs_netcdf":
            if str(a).upper() == 'NO' or str(a).upper() == 'N':
               gfs_netcdf = False
            elif str(a).upper() == 'YES' or str(a).upper() == 'Y':
               gfs_netcdf = True
            else:
                usage()
                sys.exit("Invalid choice for --gfs_netcdf. yes or no only..")
        elif o == "-h":
            usage()
            sys.exit()
        else:           
            usage()
            sys.exit("Unhandled option.")   


    if (gfs_nemsio or gfs_netcdf) and suf !="":        
        suf=""
        print("When gfs_nemsio=YES we must use hi-res EnKF members! Resetting resolution ot hi-res!")

    if fourd==None:
        # Convert cdate to a datetime object
        indate=datetime.strptime(cdate,'%Y%m%d%H')
        # Starting cdate for searching - find by looking back tm hours
        #  from closest, valid global cdate (something evenly divisible by 6)
        tm=tm+indate.hour%6
# orig will only consider to 6 h old.  What do we want for reproducibility (system could run late and use on time GFS)
#        tmlist=range(tm,0-indate.hour%6,-6)
# this version considers to on time run
#        tmlist=range(tm,-6,-6)
        tmlist=range(tm,0,-6)
        tmlist=[x for x in tmlist if x >= 0] #make sure we only keep the positive elements
        print 'tmlist is: ', tmlist
        dateobjs=[indate+timedelta(hours=-tm) for tm in tmlist]
        hdiff=999
        fsave=999
        for dateobj in dateobjs:
            cdate=dateobj.strftime('%Y%m%d%H')
            PDY=dateobj.strftime('%Y%m%d')
            CYC=dateobj.strftime('%H')
            for f in range(sfhr,72):
                if retro:
                    if 'INPUT' in os.environ:
                        path=os.environ['INPUT']
                    else:
                        sys.exit("Unable to locate INPUT in your environment!\n" \
                                 "Therefore cannot find where EnKF members reside for Retro. Exit.")
                else:
                    path=comgfs+'.'+PDY+'/'+CYC+'/atmos'

                if gfs_nemsio:
                    en=path+'/gfs.t'+CYC+'z.'+filetype+'f'+str(f).zfill(3)+'.nemsio'
#                    print 'set en to: ', en
                elif gfs_netcdf:
                    en=path+'/gfs.t'+CYC+'z.'+filetype+'f'+str(f).zfill(3)+'.nc'
#                    print 'set en to: ', en
                else:
                    en=path+'/sfg_'+cdate+'_fhr'+str(f).zfill(2)+'_ensmean'+suf
#                print 'Checking in %s for forecast hour %s' % (path,str(f).zfill(2))
                if is_non_zero_file(en):
                    age=(time.time()-os.stat(en).st_mtime)/60. #get difference in seconds and convert to minutes
                    if age > 5.:
                        # If the files exists and has not been modified for 5 minutes
                        #  let's calculate the forecast valid date and update hdiff
                        fvalid=dateobj+timedelta(hours=f)
                        tdelt=fvalid-indate
                        diff = abs( (tdelt.days * 24. ) + (tdelt.seconds/(3600.)) )
                        if exact:
                            if diff <= 0.0001 and f<=fsave:
                                nens=checkfile(path,f,cdate,suf,gfs_nemsio,gfs_netcdf)
                                if nens>=0:
                                    hdiff=diff
                                    svdate=dateobj
                                    fsave=f
                        else:
                            if diff <= hdiff or (diff <= hdiff and f<fsave):
                                nens=checkfile(path,f,cdate,suf,gfs_nemsio,gfs_netcdf)
                                if nens>=0:
                                    hdiff=diff
                                    svdate=dateobj
                                    fsave=f

        if svdate is None: 
            print 'Unable to find matching EnKF members.'
            sys.exit()
        write_filelist(fname,comgfs,fsave,svdate,retro,path,suf,o3fname,filetype,getmean,gfs_nemsio,gfs_netcdf)


if __name__ == '__main__': main()
