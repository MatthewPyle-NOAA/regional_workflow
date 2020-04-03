 SUBROUTINE POLFIXV(NM,NX,KM,RLAT,RLON,IB,LO,UO,VO)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! $Revision: 71314 $
!
! SUBPROGRAM:  POLFIXV    MAKE MULTIPLE POLE VECTOR VALUES CONSISTENT
!   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
!
! ABSTRACT: THIS SUBPROGRAM AVERAGES MULTIPLE POLE VECTOR VALUES
!           ON A LATITUDE/LONGITUDE GRID.  BITMAPS MAY BE AVERAGED TOO.
!           VECTORS ARE ROTATED WITH RESPECT TO THEIR LONGITUDE.
!        
! PROGRAM HISTORY LOG:
!   96-04-10  IREDELL
!
! USAGE:    CALL POLFIXV(NM,NX,KM,RLAT,RLON,IB,LO,UO,VO)
!
!   INPUT ARGUMENT LIST:
!     NM       - INTEGER NUMBER OF GRID POINTS
!     NX       - INTEGER LEADING DIMENSION OF FIELDS
!     KM       - INTEGER NUMBER OF FIELDS
!     RLAT     - REAL (NM) LATITUDES IN DEGREES
!     RLON     - REAL (NM) LONGITUDES IN DEGREES
!     IB       - INTEGER (KM) BITMAP FLAGS
!     LO       - LOGICAL*1 (NX,KM) BITMAPS (IF SOME IB(K)=1)
!     UO       - REAL (NX,KM) U-WINDS
!     VO       - REAL (NX,KM) V-WINDS
!
!   OUTPUT ARGUMENT LIST:
!     LO       - LOGICAL*1 (NX,KM) BITMAPS (IF SOME IB(K)=1)
!     UO       - REAL (NX,KM) U-WINDS
!     VO       - REAL (NX,KM) V-WINDS
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
 IMPLICIT NONE
!
 INTEGER,      INTENT(IN   ) :: IB(KM), NM, NX, KM
!
 LOGICAL*1,    INTENT(INOUT) :: LO(NX,KM)
!
 REAL,         INTENT(IN   ) :: RLAT(NM), RLON(NM)
 REAL,         INTENT(INOUT) :: UO(NX,KM), VO(NX,KM)
!
 REAL,         PARAMETER     :: RLATNP=89.9995
 REAL,         PARAMETER     :: RLATSP=-RLATNP
 REAL,         PARAMETER     :: PI=3.14159265358979
 REAL,         PARAMETER     :: DPR=180./PI
!
 INTEGER                     :: K, N
!
 REAL                        :: CLON(NM),SLON(NM)
 REAL                        :: TNP, UNP, VNP, WNP
 REAL                        :: TSP, USP, VSP, WSP
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 DO N=1,NM
   CLON(N)=COS(RLON(N)/DPR)
   SLON(N)=SIN(RLON(N)/DPR)
 ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 DO K=1,KM
   WNP=0.
   UNP=0.
   VNP=0.
   TNP=0.
   WSP=0.
   USP=0.
   VSP=0.
   TSP=0.
!  AVERAGE MULTIPLE POLE VALUES
   DO N=1,NM
     IF(RLAT(N).GE.RLATNP) THEN
       WNP=WNP+1
       IF(IB(K).EQ.0.OR.LO(N,K)) THEN
         UNP=UNP+(CLON(N)*UO(N,K)-SLON(N)*VO(N,K))
         VNP=VNP+(SLON(N)*UO(N,K)+CLON(N)*VO(N,K))
         TNP=TNP+1
       ENDIF
     ELSEIF(RLAT(N).LE.RLATSP) THEN
       WSP=WSP+1
       IF(IB(K).EQ.0.OR.LO(N,K)) THEN
         USP=USP+(CLON(N)*UO(N,K)+SLON(N)*VO(N,K))
         VSP=VSP+(-SLON(N)*UO(N,K)+CLON(N)*VO(N,K))
         TSP=TSP+1
       ENDIF
     ENDIF
   ENDDO
!  DISTRIBUTE AVERAGE VALUES BACK TO MULTIPLE POLES
   IF(WNP.GT.1) THEN
     IF(TNP.GE.WNP/2) THEN
       UNP=UNP/TNP
       VNP=VNP/TNP
     ELSE
       UNP=0.
       VNP=0.
     ENDIF
     DO N=1,NM
       IF(RLAT(N).GE.RLATNP) THEN
         IF(IB(K).NE.0) LO(N,K)=TNP.GE.WNP/2
         UO(N,K)=CLON(N)*UNP+SLON(N)*VNP
         VO(N,K)=-SLON(N)*UNP+CLON(N)*VNP
       ENDIF
     ENDDO
   ENDIF
   IF(WSP.GT.1) THEN
     IF(TSP.GE.WSP/2) THEN
       USP=USP/WSP
       VSP=VSP/WSP
     ELSE
       USP=0.
       VSP=0.
     ENDIF
     DO N=1,NM
       IF(RLAT(N).LE.RLATSP) THEN
         IF(IB(K).NE.0) LO(N,K)=TSP.GE.WSP/2
         UO(N,K)=CLON(N)*USP-SLON(N)*VSP
         VO(N,K)=SLON(N)*USP+CLON(N)*VSP
       ENDIF
     ENDDO
   ENDIF
 ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 END SUBROUTINE POLFIXV
