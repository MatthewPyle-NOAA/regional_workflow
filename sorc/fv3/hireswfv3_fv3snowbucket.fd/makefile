SHELL=/bin/sh
#
SRCS=	fv3bucket.f GET_BITS.f

OBJS=	fv3bucket.o  GET_BITS.o

LIBS =   ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} 
INC =  -I${G2_INC4} -I${G2TMPL_INC}
DEBUG = -fpe0 -ftrapuv -g -traceback

FFLAGS = -O2 -g -traceback -assume noold_ldout_format   

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	 clean hireswfv3_fv3bucket

$(OBJS):       
	$(COMPILER) ${INC} -c $(FFLAGS) $(@:.o=.f)

hireswfv3_fv3bucket:
	$(COMPILER) ${INC} $(FFLAGS) -o $(@) fv3bucket.f GET_BITS.f $(LIBS)
clean:
	/bin/rm   hireswfv3_fv3bucket
copy:
	/bin/cp hireswfv3_fv3bucket ../../exec/href_fv3bucket

