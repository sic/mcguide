	SUBROUTINE MCGUIDE(nwav,wave,wgxe,sgxe,syme,fille,
     *                      ftranse,delole,avrefe)

*	This is the 'latest' (8-jan-1985) version (FKGUIDE43 on the IBM).
*	The only change from FKGUIDE42 (the written up version) seems to
*	be an extra write statement putting out the total number of transmitted
*	neutrons.

*	Further modified to accomodate neutrons of Gamma > Gamma_c which are
*	incident on the sample but do not suffer reflection within the (short)
*	guide. Also, now Gamma can be different in different regions (changed
*	on 19.6.85).

*	Modifications were done to get a more user friendly program. Modified 
*	by David Martin on 10.02.93.

*	Latest modification includes the moderator flux intensity. Changed on
*	19.05.93 by David Martin.

*	To ensure the randomness of positions and velocities, the random number
*	generator used is "g05daf" from the NAG library. It uses a seed
*	provided by "g05ccf" which ensures a non-repeteable sequence. 	

*	Modified on 08.04.94 by David Martin to include the different 
*	reflections coefficients of a supermirror surface.
                  
*	Modified on 27.04.95 by Stuart Campbell to include the possibility 
*       of a polarizing bender section of guide.
*	Also different reflection coefficients for the polarizing bender
*	surfaces.
                  
*	Modifications on 19.5.95 by Stuart Campbell 
* 	"spin" of the neutrons is taken into account when calculating
*	each reflection.

*	Modified (25.11.95) by Stuart Campbell write a map of the
*	exit/sample plane to a file.
 
        IMPLICIT REAL (A-H,O-Z)
        REAL G05CBF, G05CCF, G05DAF

*       Extra definitions for VAX

	INTEGER*2 LIB$DATE_TIME, LIB$INIT_TIMER, LIB$SHOW_TIMER, ISTATUS
	INTEGER*2 HANDLE, RANDOM_ANSWER, MI
	CHARACTER*40 DATETIME, NAME
        INTEGER SPINUP,SPINDW

	REAL INT_PROB,ID
      	REAL TGX(10,10),TS(10,10),TITLE(20),MX,MZ,TXX(10),TSE(10,10)
     $         , TITL1(20),TITL2(20),TITL3(20), TGXE(10,10) 
	real wave(100), wgxe(100), sgxe(100), syme(100), fille(100), 
     *	     ftranse(100), delole(100), avrefe(100)
      	INTEGER*2 IGEOM(100,600), IREFLC(100,600), SURFLAG(600)
     *           ,NOJOB
      	COMMON/GUIDE/VL,GX,GZ,DELTA
      	COMMON/GLASS/BETA(100),ABUT(100),CREF(100),GLENG(100),
     $		     GAMMB(100),GAMMA(100)
	COMMON/POLAR/CREFINT,POL_EFF,GAMMA_BEND,GAMMB_BEND,SPIN
     $             GAMMA_BEND_NEW, CREFINT_NEW
        COMMON/ANGG/ANGLI,ANGLF,LABUT,ANGL,AP,BP,CP
      	COMMON/DTO/ISURF,EXDIST
      	COMMON/DIR/VX,VY,VZ
      	COMMON/DEF/X,Y,Z,IREG
      	COMMON/GEOM/NSURF,NREG,IGEOM,SURFLAG
      	COMMON/GEOM/A(600),B(600),C(600),D(600),E(600),F(600),G(600),
     $		    P(600),Q(600),R(600)
      	COMMON/TES/IN
      	LOGICAL IN
	EXTERNAL TAY         

*	OPEN(UNIT=19,FILE='ANG.OUT',STATUS='NEW')

      	OPEN(UNIT=3,FILE='BR.DAT',STATUS='OLD')
	OPEN(UNIT=2,FILE='MC.DAT',STATUS='NEW')
      	OPEN(UNIT=4,FILE='MC.OUT',STATUS='NEW')
      	OPEN(UNIT=7,FILE='FLUX.OUT',STATUS='NEW')
	OPEN(UNIT=5,FILE='SYMMETRY.OUT',STATUS='NEW')
      	OPEN(UNIT=9,FILE='SUMMARY.OUT',STATUS='NEW')
      	OPEN(UNIT=11,FILE='EXIT.OUT',STATUS='NEW')
      	OPEN(UNIT=13,FILE='SAMPLE.OUT',STATUS='NEW')
*      	OPEN(UNIT=14,FILE='FLUX.OUT',STATUS='NEW')
	OPEN(UNIT=21,FILE='GRID.SAM',STATUS='NEW')
	OPEN(UNIT=31,FILE='GRID.EXI',STATUS='NEW')


*	This file is FKMAINDR.GUIDES.SIMFTN. It was last modified on 21.5.79
*	......MWJ

* ====> Remember 1 st surface must be guide entrance, while 2 nd surface must
*	be sample plane in the coordinate system situated at the guide exit.

*	Warm up random generator here

	DO I = 1, 1000
		CALL G05DAF(-1.,1.)
	ENDDO                  

*	Matrix limits
	MI = 10
	
*      	Date time on the VAX

       	ISTATUS = LIB$DATE_TIME(DATETIME)
       	HANDLE  = 0
       	ISTATUS = LIB$INIT_TIMER(HANDLE)

	READ(3,112)NAME 
	READ(3,*)
	READ(3,*)
       	READ(3,100) NHIST,NSURF,NREG

       	DO I = 1, NSURF
			READ(3,*)
			READ(3,*)
			READ(3,*)
*     			READ(3,109) A(I),B(I),C(I),D(I),E(I)
*      			READ(3,109) F(I),G(I),P(I),Q(I),R(I)
     			READ(3,*) A(I),B(I),C(I),D(I),E(I)
      			READ(3,*) F(I),G(I),P(I),Q(I),R(I)
	ENDDO

	READ(3,*)
	READ(3,*)
       	DO I = 1, NREG
		READ(3,*)
		READ(3,*)
     		READ(3,102)(IGEOM(I,J), J = 1, NSURF)
     	ENDDO

	READ(3,*)
	READ(3,*)
	READ(3,*)
	READ(3,*)
	READ(3,*)

       	DO I = 1, NREG
		READ(3,*)
		READ(3,*)
	     	READ(3,102) (IREFLC(I,J),J = 1, NSURF)
	ENDDO

	READ(3,*) 
	READ(3,*) 
	READ(3,*) 
       	READ(3,101) VL,GX,GZ
	READ(3,*) 
	READ(3,*) 
	READ(3,108) MX,MZ
	READ(3,*) 
	READ(3,*) 
	READ(3,108) SX,SZ
	READ(3,*) 
	READ(3,*) 
	READ(3,*) 
       	READ(3,130) THETA,X1,Y1
	READ(3,*) 
	READ(3,*) 
	READ(3,*) 
       	READ(3,104) NWAV,WAV0,DWAV
	READ(3,*) 
	READ(3,*) 

	DO I = 1, NREG
		READ(3,*) 
		READ(3,*) 
       		READ(3,107) BETA(I), CREF(I), ABUT(I), GLENG(I), GAMMB(I)
	ENDDO

	READ(3,*)
	READ(3,*)
       	READ(3,99) FI
	READ(3,*)
	READ(3,*)
	READ(3,*)
*       READ(3,99) BEETA
       	READ(3,99) CONE

*	Reading in values for SURFLAG

	READ(3,*)
	READ(3,*)
	READ(3,102) (SURFLAG(I), I=1,NSURF)

*	Reading in the critical angle per unit wavelength for 
*	for the polarizing bender surfaces

	READ(3,*)
*	READ(3,*)
	READ(3,*)
	READ(3,*) GAMMB_BEND, CREFINT, POL_EFF

       	CLOSE(3)

*  	Output for VAX
	
      	WRITE(*,214) DATETIME, NAME
      	WRITE(2,214) DATETIME, NAME
	WRITE(7,112) NAME
	WRITE(9,112) NAME
*	WRITE(11,112) NAME
*	WRITE(13,112) NAME
      	WRITE(*,200) NHIST, NSURF, NREG
      	WRITE(2,200) NHIST, NSURF, NREG
	WRITE(2,*)
	WRITE(2,202) 
     	WRITE(2,203) VL, GX, GZ, MX, MZ, SX, SZ
	WRITE(2,*)
	WRITE(2,201) 
      	WRITE(2,217) THETA, X1, Y1
	WRITE(2,*)
	WRITE(2,206)
      	WRITE(2,209) NWAV, WAV0, DWAV
	WRITE(2,*)
      	WRITE(2,230) FI
	WRITE(2,*)
      	WRITE(2,231) CONE
      	WRITE(2,215)

	DO I = 1, NREG
	      	WRITE(2,220) I, BETA(I), CREF(I), ABUT(I), 
     $			     GLENG(I), GAMMB(I)
	ENDDO

      	COST = DCOS(THETA)
      	SINT = DSIN(THETA)

*	POL_EFF = POL_EFF/100

*	Converting "FI" from degrees to radians.

      	FI = FI * 0.017453293
      	TFI = DTAN(FI)
      	CFI = DCOS(FI)

*	Initialize accumulators to zero

      	EPSI = 0.01
      	TIMEA = 0.0

*	do i = 1, ntimes

      	DO 11 IWAV = 1, NWAV
       		WAV = (IWAV-1) * DWAV + WAV0
      		NIL = 0
      		NENTL = 0
      		NSTART = 0
      		WGX = 0.0
      		WNPATH = 0.0
      		WNREFT = 0.0
      		WLOSS = 0.0
                WABSORB = 0.0
                NABSORB = 0
      		DO I = 1, 10
      			DO J = 1, 10
      				TGX(I,J) = 0.0
   				TS(I,J) = 0.0
      				TGXE(I,J) = 0.0
   				TSE(I,J) = 0.0
			ENDDO
		ENDDO
                   
	SPINDW = 0
	SPINUP = 0


*	Set the critical angle.


		GAMMA_BEND = GAMMB_BEND * WAV

		PI = 3.14159265359
      		DO I = 1, NREG
      			GAMMA(I) = GAMMB(I) * WAV
		ENDDO       

*	The following possibility can be of interest mainly in the case of an
*	angular dependent reflectivity or a straight guide. 

*		CONE = (CONE_ANGLE*PI)/(GAMMA(1)*WAV*180.) 

        	DELTA = CONE * GAMMA(1)
		PHIWAV = TAY(WAV)*(9.0668e-04)
*		PHIWAV = 1.e+5
        	WTC = GX * GZ * 4.0 * DELTA * DELTA * PHIWAV
        	VLM1 = 0.
        	VLM2 = 0.
        	NFINI = 0

	        	DO 6 IHIST = 1, NHIST
	IF(IHIST/1000.EQ.IHIST/1000.)THEN
		WRITE(*,*) 'NEUTRON HISTORY NUMBER:  ',IHIST
	ENDIF
			WT = WTC
        		VLHIST = 0.
        		NREF   = 0
        		CALL START

*	PRINT*,'CALLING START'

*	We now have the starting coordinates: X, Y, Z, VX, VY and VZ on a plane
*	at the guide entrance. We first see if it will enter the guide system.

		IF(G05DAF(0.,1.).GT.0.5) THEN
			SPIN = 0.5
		ELSE
			SPIN = -0.5
		ENDIF
		
*	PRINT*,SPIN



        		XE = X
        		YE = Y
        		ZE = Z
        		X = X + VX * EPSI
        		Y = Y + VY * EPSI
        		Z = Z + VZ * EPSI
        		CALL WHICHR
        		IF(IREG.EQ.0) GO TO 62

*	We now have an acceptable neutron. Now trace its trajectory back to 
*	the moderator plane to see if it was available.

      			VP = - (YE - XE*TFI) / (VY - VX*TFI)
      			XM = XE + VP*VX
      			ZM = ZE + VP*VZ
      			IF(DABS(XM).GT.(MX*CFI/2.0)) GO TO 61
      			IF(DABS(ZM).GT.(MZ/2.0))  GO TO 61
      			VLHIST = EPSI
      			IREG = 0
      			NSTART = NSTART+1
3   			LASTRG = IREG
*	print*,'point1:',ireg
      			CALL WHICHR
*        print*,'point2:',ireg
      	
*	Branch if neutron is outside guide

      			IF(IREG.EQ.0) then
			GOTO 20
			ENDIF

4  			CALL DTOEX

*	IF(SURFLAG(ISURF).gt.0.0) THEN
*	IREG = 13
*	ENDIF
			CALL TESTIN
       			IF(IN) GO TO 12

*	Here for a pathological way
			
      			WNPATH = WNPATH + WT
      			GO TO 6
12  	CONTINUE
                 
*	PRINT*,IREG,isurf
 	
*	IF(IREG.GE.13.0.AND.SPIN.LT.0.) THEN
*	PRINT*,'SPIN =',SPIN,' IREG =',IREG,' ISURF=',ISURF
*	ENDIF                                 

*	IF(SURFLAG(ISURF).gt.0.0)THEN
*	PRINT*,'Collision with an internal surface'
*	PRINT*,'REGION: ',IREG,' and SURFACE:',ISURF
*	ENDIF


      	IF(IREFLC(IREG,ISURF).EQ.1.0) GO TO 10
	IF(SURFLAG(ISURF).gt.0.0) GOTO 10	

*	the next few lines move the neutron along vx,vy,vz to the
*	edge of the guide then a small distance epsi to take neutron 
*	outside the guide
5   	X = X + VX * (EXDIST + EPSI)
      	Y = Y + VY * (EXDIST + EPSI)
        Z = Z + VZ * (EXDIST + EPSI)
        VLHIST = VLHIST + EXDIST + EPSI
        GO TO 3

*	Here for a reflection. We first transport neutron to point on surface, 
*	then use subtended angle to check if it is in allowed angular range. 
*	If permitted Subroutine RFN is used to reflect the neutron direction
*	cosines 

10 	CONTINUE
      	X = X + VX*EXDIST
      	Y = Y + VY*EXDIST
      	Z = Z + VZ*EXDIST

      	CALL WOBBLE

*  	VAX double precision ARC SINE

      	ANGL = DASIN(AP*VX + BP*VY + CP*VZ)

                                     
*	if(ireg.ne.1000.0) goto 16261

*	Supermirror reflectivity coefficient treatment for each region. 
*	Based on PSI results (and hopes!) on 08.04.94.

	DO J = 1, NREG
* 	m = 2.0
	   IF(GAMMB(J).EQ.0.00346)THEN
	      IF(DABS(ANGL).GT.GAMMA(IREG)/2.0)THEN
		 CREF(J) = 0.92
	      ELSE
		 CREF(J) = 0.999
	      ENDIF
	   ENDIF

*	m = 2.4
	   IF(GAMMB(J).EQ.0.004152)THEN
	      IF(DABS(ANGL).GT.GAMMA(IREG)/1.2)THEN
		 CREF(J) = 0.85
	      ELSE
		 IF(DABS(ANGL).GT.GAMMA(IREG)/2.0)THEN
		    CREF(J) = 0.92
		 ELSE
		    IF(DABS(ANGL).GT.GAMMA(IREG)/2.4)THEN
		       CREF(J) = 0.995
		    ELSE
		       CREF(J) = 0.999
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF

*	m = 3.0
	   IF(GAMMB(J).EQ.0.00519)THEN
	      IF(DABS(ANGL).GT.GAMMA(IREG)/1.25)THEN
		 CREF(J) = 0.80
	      ELSE
		 IF(DABS(ANGL).GT.GAMMA(IREG)/1.5)THEN
		    CREF(J) = 0.85
		 ELSE
		    IF(DABS(ANGL).GT.GAMMA(IREG)/1.88)THEN
		       CREF(J) = 0.92
		    ELSE
		       CREF(J) = 0.999
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF

*	m = 4.0
	   IF(GAMMB(J).EQ.0.00692)THEN
	      IF(DABS(ANGL).GT.GAMMA(IREG)/1.333333)THEN
		 CREF(J) = 0.60
	      ELSE
		 IF(DABS(ANGL).GT.GAMMA(IREG)/1.666667)THEN
		    CREF(J) = 0.80
		 ELSE
		    IF(DABS(ANGL).GT.GAMMA(IREG)/2.)THEN
		       CREF(J) = 0.85
		    ELSE
		       IF(DABS(ANGL).GT.GAMMA(IREG)/2.507)THEN
			  CREF(J) = 0.92
		       ELSE
			  CREF(J) = 0.999
		       ENDIF
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF
	ENDDO

*	End of the supermirror reflectivity coefficient treatment.
                                     

*	start of supermirror treatment for polarizing surfaces

* 	m = 2.0
	   IF(GAMMB_BEND.EQ.0.00346)THEN
	      IF(DABS(ANGL).GT.GAMMA_BEND/2.0)THEN
		 CREFINT = 0.92
	      ELSE
		 CREFINT = 0.999
	      ENDIF
	   ENDIF

*	m = 2.4
	   IF(GAMMB_BEND.EQ.0.004152)THEN
	      IF(DABS(ANGL).GT.GAMMA_BEND/1.2)THEN
		 CREFINT = 0.85
	      ELSE
		 IF(DABS(ANGL).GT.GAMMA_BEND/2.0)THEN
		    CREFINT = 0.92
		 ELSE
		    IF(DABS(ANGL).GT.GAMMA_BEND/2.4)THEN
		       CREFINT = 0.995
		    ELSE
		       CREFINT = 0.999
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF

*	m = 3.0
	   IF(GAMMB_BEND.EQ.0.00519)THEN
	      IF(DABS(ANGL).GT.GAMMA_BEND/1.25)THEN
		 CREFINT = 0.80
	      ELSE
		 IF(DABS(ANGL).GT.GAMMA_BEND/1.5)THEN
		    CREFINT = 0.85
		 ELSE
		    IF(DABS(ANGL).GT.GAMMA_BEND/1.88)THEN
		       CREFINT = 0.92
		    ELSE
		       CREFINT = 0.999
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF

*	m = 4.0
	   IF(GAMMB_BEND.EQ.0.00692)THEN
	      IF(DABS(ANGL).GT.GAMMA_BEND/1.333333)THEN
		 CREFINT = 0.60
	      ELSE
		 IF(DABS(ANGL).GT.GAMMA_BEND/1.666667)THEN
		    CREFINT = 0.80
		 ELSE
		    IF(DABS(ANGL).GT.GAMMA_BEND/2.)THEN
		       CREFINT = 0.85
		    ELSE
		       IF(DABS(ANGL).GT.GAMMA_BEND/2.507)THEN
			  CREFINT = 0.92
		       ELSE
			  CREFINT = 0.999
		       ENDIF
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF

16261   continue

*	End of supermirror treatment for polarizing surfaces
****************************************************************

*	in the next few lines maybe add the check for surflag

*	If surflag of surface isurf that neutron is incident with is
*	not equal to 1 then ==> normal surface.

*	If surface isn't a vertical along the length
	IF(SURFLAG(ISURF).EQ.0) GOTO 22
                                                    
****************************************************************	
*	Treatment for considering glass thickness.
                                    
*	Surflag for a Polarizing = 2
*	Surflag for a Normal = 1 
                  
	IF(SURFLAG(ISURF).EQ.1.0) THEN
*	===> NORMAL
		IF(ANGL.LT.0.0) GOTO 22

		IF(ANGL.GT.0.0) THEN
*	Ending neutron history (inside glass)	
	        GOTO 21
        	ENDIF
	ENDIF

        IF(SURFLAG(ISURF).EQ.2.0) THEN   
*	===> POLARIZING
        	IF(ANGL.GT.0.0) GOTO 24

		IF(ANGL.LT.0.0) THEN
*	Ending neutron history (inside glass)                
		GOTO 21
		ENDIF
	ENDIF
        
	print*,'I don t know what has happened'

****************************************************************
*	IF(ANGL.EQ.0.) THEN 
*      	PRINT*,'ANGL=0'
*	GOTO 21
*	ENDIF

*	IF(ANGL.GT.0.) THEN 
* 	GOTO 24
*	ENDIF

*	IF(ANGL.LT.0.) THEN
* 	GOTO 22
*	ENDIF
*****************************************************************
                                                                
*	Reflection treatment using polarizing surface parameters

24      CONTINUE

*	Removing this line means that all polarizing surfaces will "polarize"
	IF(POL_EFF.EQ.0.0) GOTO 22

*	PRINT*,'Reflection from a polarizing surface'
	
	GAMMA_BEND_NEW = GAMMA_BEND
	CREFINT_NEW = CREFINT

*	print*,'spin of n in polarizing surface:',spin

	IF(SPIN.LT.0) THEN
*	PRINT*,'SPIN IS DOWN ==> ENDING'
	GOTO 21
	ENDIF

*	An alternative way of doing the above.

*	IF(SPIN.LT.0.0) THEN
*	GAMMA_BEND_NEW = GAMMA_BEND*0.0
*	CREFINT_NEW = 0.0
*	ENDIF

*	IF(DABS(ANGL).GT.GAMMA_BEND_NEW) GOTO 21
	LABUT=1
	IF(ABUT(IREG).NE.0.) CALL ABUTL2
	IF(LABUT.EQ.0) GOTO 21

        CALL RFN
	NREF = NREF + 1
	WT = WT*CREFINT_NEW
	WLOSS = WLOSS + WT*(1.0 - CREFINT_NEW)
        GOTO 19		

***************************************************************
*	Normal reflection off a surface 

22	CONTINUE

*	This line means that all surfaces are polarizing
*	IF(SPIN.LT.0) GOTO 21
		
        IF(DABS(ANGL).GT.GAMMA(IREG)) GOTO 21
	LABUT=1
	IF(ABUT(IREG).NE.0.) CALL ABUTL2
	IF(LABUT.EQ.0) GOTO 21

	CALL RFN
	NREF = NREF + 1
	WT = WT*CREF(IREG)
	WLOSS = WLOSS + WT*(1.0 - CREF(IREG))
                                              

*****************************************************************

19	CONTINUE	

*	IF(SPIN.LT.0.) THEN
*	PRINT*,'IREG=',IREG,' ISURF=',ISURF
*	ENDIF


*	Move neutron by "epsi" in the new direction to take them away from the 
*	surface

        X = X + EPSI*VX
        Y = Y + EPSI*VY
        Z = Z + EPSI*VZ
        VLHIST = VLHIST + EXDIST + EPSI
	GOTO 4

*	Here when neutron outside guide assembly.
*	If neutron is leaving the highest numbered segment through a
*	non-reflecting surface, then the end of the guide has been reached.
*	Otherwise the neutron is assumed to have left through the side of the
*	guide.

20      CONTINUE

        IF(LASTRG.EQ.NREG) THEN 
*        PRINT*,'NREG=',NREG
*	PRINT*,'LASTRG=',LASTRG
*	PRINT*,'LASTRG=NREG ==> LEAVING GUIDE'
        GO TO 30 
	ENDIF

21      WLOSS = WLOSS + WT
*	PRINT*,'NEUTRON LOST'
        GO TO 63

*	Here when leaving the guide exit (gx)

30      CONTINUE

        XD = (X-X1)*COST - (Y-Y1)*SINT
        YD = (X-X1)*SINT + (Y-Y1)*COST
        VXD= VX*COST - VY*SINT
        VYD = VX*SINT + VY*COST
        X = XD
        Y = YD
        VX = VXD
        VY = VYD


        WGX = WGX + WT


*	Check the spin state of the neutrons

*	print*,'spin is ',spin

	IF(SPIN.EQ.0.5) THEN
*	print*,'spin is +ve'	
	SPINUP = SPINUP + 1
	ENDIF

	IF(SPIN.LT.0.) THEN
	SPINDW = SPINDW + 1
*	print*,'spin is -ve'
	ENDIF


*	Now bin the neutrons at the guide exit.

        CALL BIN(WT,X,Z,GX,GZ,TGX,MI,MI)

*	Now transport to the sample plane.

        DD = DIST(2)

        VLHIST = VLHIST + DD
        X = X + VX*DD
        Z = Z + VZ*DD

*	PRINT*,'NEUTRON @ SAMPLE'

*	Final check on spin
	
*	IF(POL_EFF.EQ.1.0.AND.SPIN.EQ.-0.5) THEN
*	PRINT*,'There is an up spin at the sample plane!!!'
*	ENDIF


*	Now bin neutrons at sample plane.

        CALL BIN(WT,X,Z,SX,SZ,TS,MI,MI)

        WNREFT = WNREFT + NREF*WT
        VLM1 = VLM1 + VLHIST*WT
        VLM2 = VLM2 + VLHIST*VLHIST*WT
        GO TO 63
61      NIL =NIL +1
        GO TO 63
62      NENTL = NENTL + 1
63      CONTINUE
6	ENDDO

      	IF(WGX.NE.0)GO TO 177
      	VLM1=VLHIST*WTC
      	VLM2=VLHIST*VLHIST*WTC
      	WGX=WTC


      	WRITE(6,178)
      	WRITE(2,178)
178     FORMAT(1X,'FOR THIS WAVE LENGTH NO NEUTRON WAS TRANSMITTED')
177     CONTINUE

*	Above three lines inserted on 07.08.1985

      	WRITE(*,219)
      	WRITE(2,219)

	PRINT*,'NSTART:',NSTART
        PRINT*,'NENTL :',NENTL

        SIGMAL = DSQRT((VLM2 - (VLM1*VLM1/WGX))/WGX)
        DELOL = (SIGMAL*WGX)/VLM1
        FILL = (1.0*NSTART/(NHIST-NENTL))
        FTRANS = WGX/(NSTART*WTC)

        DO 7 I=1,MI
       		DO 7 J=1,MI
       		TGX(I,J) = TGX(I,J)/NHIST
7    		TS(I,J) = TS(I,J)/NHIST

        DO 16 I=1,MI
        	TXX(I) = 0.0
      		DO 15 J=1,MI
15  		TXX(I) = TXX(I) + TGX(I,J)
16  		WRITE(4,1400)TXX(I)
      
*	PRINT*,'WABSORB = ',WABSORB
*        PRINT*,'NO. ABSORBED =',NABSORB

	AVREF = WNREFT/WGX
        NPATH = WNPATH/WTC
        NLOST = WLOSS/WTC
        NFINI = WGX/WTC
        WGX = WGX/NHIST

        WRITE(6,208)
        WRITE(2,208)
        WRITE(6,210) WAV,NENTL,MX,NIL,MZ,NSTART,GX,NLOST,GZ,NFINI,
     $  SX,FILL,SZ,FTRANS,DELOL,AVREF,NPATH
        WRITE(2,210) WAV,NENTL,MX,NIL,MZ,NSTART,GX,NLOST,GZ,NFINI,
     $  SX,FILL,SZ,FTRANS,DELOL,AVREF,NPATH
        WRITE(4,1200)WAV
        WRITE(4,1300)FTRANS
        WRITE(4,1200)AVREF
        WRITE(6,211)
        WRITE(2,211)
*	These next two lines write the matrix to the screen/mc.dat
*	for the guide exit
	WRITE(31,204) ((TGX(I,J),I=1,MI),J=1,MI)
                
	WRITE(6,204) ((TGX(I,J),I=1,MI),J=1,MI)
        WRITE(2,204) ((TGX(I,J),I=1,MI),J=1,MI)
        WRITE(6,207) TXX, WGX	
        WRITE(2,207) TXX, WGX

*	WGX = 0.0
	SGX = 0.0      	
	ESGX = 0.0      	
	DO I=1,MI
      		TXX(I) = 0.0
      	      	DO J=1,MI
  			TXX(I) = TXX(I) + TS(I,J)
		ENDDO 
  		WRITE(4,1400)TXX(I)
		SGX = SGX + TXX(I)
	ENDDO
	
***	Calculating and writing the symmetry at sample plane

	DO I = 1, (MI/2)
		TXXL = TXXL + TXX(I)
		TXXR = TXXR + TXX(I + (MI/2))
	ENDDO
	
	PRINT*,'TXXR =',TXXR
	PRINT*,'TXXL =',TXXL

*	if(txxr.eq.0.or.txxl.eq.0) goto 11
	if(txxr.eq.0.or.txxl.eq.0) goto 13746

	SYM = (TXXR/TXXL)*100.

13746   CONTINUE

	print*,'sym = ',sym
        
	IF(IWAV.EQ.1)THEN
		WRITE(5,303)
		WRITE(5,*)
	ENDIF
	WRITE(5,304) WAV, SYM 

***
	WRITE(21,204) ((TS(I,J),I=1,MI),J=1,MI)
        
	WRITE(6,204) ((TS(I,J),I=1,MI),J=1,MI)
      	WRITE(2,204) ((TS(I,J),I=1,MI),J=1,MI)
      	WRITE(6,212) TXX, SGX
      	WRITE(2,212) TXX, SGX

***	Calculating the flux through the sample.

	FSGX = SGX/(SX*SZ)
	WRITE(2,412) FSGX
	FSGX = FSGX * 1.E+06

*	PRINTING THE NUMBER OF NEUTRONS WITH EACH SPIN STATE
	PRINT*,'UP SPINS   =',SPINUP
	PRINT*,'DOWN SPINS =',SPINDW


*	IF(IWAV.EQ.1)THEN
*		WRITE(14,314)
*		WRITE(14,*)
*	ENDIF

*	WRITE(14,414) WAV, FSGX

***        

*  	Calculation of elapsed time etc... for VAX

	WRITE(6,216) 
	ISTATUS = LIB$SHOW_TIMER(HANDLE)

	WAV = WAV 
	WGX = WGX * 1.E+06 
	SGX = SGX * 1.E+06 

	IF(IWAV.EQ.1)THEN
		WRITE(7,300)
		WRITE(7,*)
		WRITE(9,310)
		WRITE(9,*)
*		WRITE(11,320)
*		WRITE(13,330)
	ENDIF
	WRITE(7,305) WAV, WGX, SGX
	WRITE(9,315) WAV, FILL, FTRANS, DELOL, AVREF	

	wave(iwav) = wav
	wgxe(iwav) = wgx
	sgxe(iwav) = sgx
	syme(iwav) = sym
	fille(iwav) = fill
	ftranse(iwav) = ftrans
	delole(iwav) = delol
	avrefe(iwav) = avref

	DO I = 1, 10
		DO J = 1, 10
			TGXE(I,J) = TGX(I,J) * 1.E+06
			TSE(I,J)  = TS(I,J) * 1.E+06
                ENDDO
	ENDDO

*	WRITE(11,*)
*	WRITE(11,324)WAV
*	WRITE(11,*)
*	WRITE(13,*)
*	WRITE(13,324)WAV
*	WRITE(13,*)

*	DO I = 1, 10
*		DO J = 1, 10
*			WRITE(11,325) I, J, TGXE(I,J)
*			WRITE(13,335) I, J, TSE(I,J)
*                ENDDO
*	ENDDO
11	ENDDO

*	enddo

      	REWIND(2)
      	REWIND(4)
	REWIND(5)
      	REWIND(7)
	REWIND(9)
	REWIND(11)
	REWIND(13)
*	REWIND(14)
        REWIND(19)
	REWIND(21)
	REWIND(31)

99  	FORMAT(F10.3)
100  	FORMAT(3I10)
101  	FORMAT(3F10.3)
130  	FORMAT(D18.11,2F14.7)
108  	FORMAT(2F10.3)
102  	FORMAT(40I2)
103  	FORMAT(F5.2)
104  	FORMAT(I10,2F10.2)
106  	FORMAT(20A4)
107  	FORMAT(5F15.6)
155  	FORMAT(20A4)
109  	FORMAT(5E15.6)
200  	FORMAT(/5X,'NO. OF NEUTRON HISTORIES =',I8/
     $ 		5X,'NO. OF SURFACES =',I8/
     $ 		5X,'NO. OF REGIONS = ',I8)
201  	FORMAT('ANGLE WITH RESPECT TO "Y" AXIS AND',/,
     $	       ' (X,Y) COORDINATES OF GUIDE EXIT.')
202	FORMAT('"Y" COORDINATE OF GUIDE ENTRANCE, GUIDE WIDTH',  
     $	       ' AND GUIDE HEIGHT.',/,'MODERATOR WIDTH AND HEIGHT,',
     $         ' SAMPLE WIDTH AND HEIGHT.')
203  	FORMAT(5X,7F10.2)
204  	FORMAT(/2X,10F7.1)
206	FORMAT('NUMBER OF WAVELENGTHS, FIRST WAVELENGTH AND WAVELENGTH',
     $         ' INCREMENT.')
207  	FORMAT(/2X,10F7.1,5X/5X,'TOTAL NEUTRONS/SEC =',F14.7,' x10^6'/)
208  	FORMAT(1H1)
209  	FORMAT(5X,I8,2F8.2)
210  	FORMAT(5X,'WAVELENGTH =',F6.2,' ANGSTROMS',
     $ 	T45,' NENTL =',I6/
     $ 	5X,'MODERATOR  X-DIMENSION =',F6.2,' CM',
     $ 	T45,'   NIL =',I6/
     $ 	5X,'MODERATOR  Z-DIMENSION =',F6.2,' CM',
     $ 	T45,'NSTART =',I6/
     $ 	5X,'GUIDE      X-DIMENSION =',F6.2,' CM',
     $ 	T45,' NLOST =',I6/
     $ 	5X,'GUIDE      Z-DIMENSION =',F6.2,' CM',
     $ 	T45,' NFINI =',I6/
     $ 	5X,'SAMPLE     X-DIMENSION =',F6.2,' CM',
     $ 	T45,' ILLUMINATION =',F8.5/
     $ 	5X,'SAMPLE     Z-DIMENSION =',F6.2,' CM',
     $ 	T45,' TRANSMISSION =',F8.5/
     $ 	T45,'    DL/L =',E12.3/
     $ 	T45,' NO. REFN/NEUT =',F6.2/
     $ 	T45,' NO. PATH. TRAJ.=',I6)
211  	FORMAT(5X,' NEUTRON INTENSITIES IN M. NEUTRONS/S CROSSING BIN '/
     $ 'ASSUMING  1.0E11  N.CM-2.STER-1 ON MODERATOR SURFACE')
212  	FORMAT(/2X,10F7.1,5X/5X,'TOTAL NEUTRONS/SECOND AT SAMPLE =',
     $	F14.7,'x10^6'/)
412  	FORMAT(/5X,'TOTAL FLUX AT SAMPLE =',
     $	F14.7,'x10^6'/)
112	FORMAT(A40)
214  	FORMAT(/' Date & time of run: ',A20/A20)
215 	FORMAT(/' REG. SURF.IRR.  REF.COEF.  ABUT.S.D.  GLASS LENGTH')
216 	FORMAT(/' This step used the following:')
217 	FORMAT(3F20.10)
219  	FORMAT(/,' MCGUIDE : MK 5.0 Grid  15/9/80')
220 	FORMAT(I4,F10.5,F10.5,2X,F10.5,2X,F10.3,F10.5)
230 	FORMAT(1X,'ANGLE OF GUIDE TO MODERATOR NORMAL=',F5.2)
231 	FORMAT(1X,'WE ACCEPT NEUTRONS WITHIN AN ANGULAR RANGE OF',F5.2,/
     $        ' TIMES THE CRITICAL ANGLE')
300	FORMAT(4X, 'WAVELENGTH', 8X, 'N/SEC', 10X, 'N/SEC @ SAMPLE') 
303	FORMAT(4X, 'WAVELENGTH', 8X, 'SYMMETRY')
314	FORMAT(4X, 'WAVELENGTH', 10X, 'FLUX @ SAMPLE')
304	FORMAT(5X, F7.3, 10X, F7.3)
305	FORMAT(5X, F7.3, 5X, E13.6, 7X, E13.6) 
414	FORMAT(5X, F7.3, 11X, E13.6)
310	FORMAT(4X, 'WAVELENGTH', 2X, 'ILLUMINATION', 2X, 'TRANSMISSION', 7X,
     $	      'DL/L', 4X, 'REFLECTIONS')
315	FORMAT(5X, F7.3, 7X, F6.4, 8X, F6.4, 6X, E10.3, 5X, F5.2) 
320	FORMAT(7X, 'I', 13X, 'J', 21X, 'BINNED FLUX @ GUIDE')
324	FORMAT(5X,'WAVELENGTH:   ', F6.2)
325	FORMAT(4X, I4, 10X, I4, 24X, E13.6)
330	FORMAT(7X, 'I', 13X, 'J', 21X, 'BINNED FLUX @ SAMPLE')
335	FORMAT(4X, I4, 10X, I4, 24X, E13.6)
1200 	FORMAT (F6.2)
1300 	FORMAT (F8.5)
1400 	FORMAT (F7.1)    

	RETURN
      	END


***********************************************


        SUBROUTINE START
        IMPLICIT REAL (A-H,O-Z)
        REAL G05DAF
        COMMON/GUIDE/VL,GX,GZ,DELTA
        COMMON/DIR/VX,VY,VZ
        COMMON/DEF/X,Y,Z,IREG
	X = G05DAF(-1.,1.)*GX/2.0
        Y = VL
	Z = G05DAF(-1.,1.)*GZ/2.0
        VX = DSIN(G05DAF(-1.,1.)*DELTA)
        VZ = DSIN(G05DAF(-1.,1.)*DELTA)
        VY = DSQRT(1.0 - VX*VX -VZ*VZ)
        RETURN
        END


***********************************************


*	Finds which region X,Y,Z is in & returns region in IREG.
*	If more than one region IREG set to -1
*	If no region, returns IREG = 0

        SUBROUTINE WHICHR
        IMPLICIT REAL (A-H,O-Z)
        INTEGER*2 IGEOM(100,600), SURFLAG(600)
        COMMON/TES/IN
        COMMON/DEF/X,Y,Z,IREG
        COMMON/GEOM/NSURF,NREG,IGEOM,SURFLAG
        COMMON/GEOM/A(600),B(600),C(600),D(600),E(600),F(600),G(600)
     $  ,P(600),Q(600),R(600)
        LOGICAL IN,FOUND
        FOUND=.FALSE.
        IT=0
        DO 1 IREG=1,NREG
        CALL TESTIN
        IF(.NOT.IN) GOTO 1
        IF(IN.AND.FOUND) GOTO 2
        IT=IREG
        FOUND=.TRUE.
1  	CONTINUE
      	IREG=IT
      	RETURN
2  	IREG=-1
      	RETURN
      	END

*	Tests whether a point X,Y,Z is in region IREG


***********************************************


        SUBROUTINE TESTIN
        IMPLICIT REAL (A-H,O-Z)
        INTEGER*2 IGEOM(100,600), SURFLAG(600)
        COMMON/TES/IN
        COMMON/DEF/X,Y,Z,IREG
        COMMON/GEOM/NSURF,NREG,IGEOM,SURFLAG
        COMMON/GEOM/A(600),B(600),C(600),D(600),E(600),F(600),G(600)
     $  ,P(600),Q(600),R(600)
        LOGICAL IN
        IN=.TRUE.
        DO 1 I=1,NSURF

        IF(IGEOM(IREG,I).EQ.0) GOTO 1

        PP=A(I)*X*X+B(I)*X+C(I)*Y*Y+D(I)*Y+E(I)*Z*Z+F(I)*Z+G(I)
     $     +P(I)*X*Y+Q(I)*Y*Z+R(I)*X*Z
        IF(PP*IGEOM(IREG,I).LT.0) GOTO 2
1       CONTINUE
        RETURN
2       IN=.FALSE.
        RETURN
        END


***********************************************


        SUBROUTINE WOBBLE
        IMPLICIT REAL (A-H,O-Z)
        REAL G05DAF
        REAL EXDIST
        INTEGER*2 IGEOM(100,600), SURFLAG(600)
        COMMON/GLASS/BETA(100),ABUT(100),CREF(100),GLENG(100),GAMMB(100)
     $  ,GAMMA(100)
        COMMON/DTO/ISURF,EXDIST
        COMMON/DIR/VX,VY,VZ
        COMMON/DEF/X,Y,Z,IREG
        COMMON/GEOM/NSURF,NREG,IGEOM,SURFLAG
        COMMON/GEOM/A(600),B(600),C(600),D(600),E(600),F(600),G(600)
     $  ,P(600),Q(600),R(600)
        COMMON/ANGG/ANGLI,ANGLF,LABUT,ANGL,AP,BP,CP

*	Let the equation of the local plane T X,Y,Z be 
*	AP*X + BP*Y + CP*Z +DP = 0

        I = ISURF
        AP = 2*A(I)*X + B(I) + P(I)*Y + R(I)*Z
        BP = 2*C(I)*Y + D(I) + P(I)*X + Q(I)*Z
        CP = 2*E(I)*Z + F(I) + Q(I)*Y + R(I)*X
        CALL NORM(AP,BP,CP)
        DOTP = AP*VX + BP*VY + CP*VZ
        ANGL = DABS(DASIN(DOTP))
        ANGLI = ANGL
        ANGLF = ANGL

*	ANGLI is the incident angle to the perfect surface
*	ANGLF is the incident angle to the perfect surface
*	This will be changed in the subsequent code if BETA.NE.0
	
        IF(BETA(IREG))2,2,1
1       CONTINUE
        XL = VX - AP*DOTP
        YL = VY - BP*DOTP
        ZL = VZ - CP*DOTP
        CALL NORM(XL,YL,ZL)
        ANGLD2 = ANGL/2.0
        ANGLM = DMIN1(ANGLD2,BETA(IREG))
        DEV = (BETA(IREG)+ANGLM)*G05DAF(0.,1.) - ANGLM
        ANGLF = ANGLI + DEV*2.0

*	The next line changes DEV according to the surface normal direction
*	with respect to the incident neutron.

        DEV = DEV*DOTP/DABS(DOTP)
        DXL = XL*DEV
        DYL = YL*DEV
        DZL = ZL*DEV
        XM = VY*CP - VZ*BP
        YM = VZ*AP - VX*CP
        ZM = VX*BP - VY*AP
        DEV = BETA(IREG)*G05DAF(-1.,1.)
        DXM = XM*DEV
        DYM = YM*DEV
        DZM = ZM*DEV
        AP = AP + DXL + DXM
        BP = BP + DYL +DYM
        CP = CP + DZL + DZM
        CALL NORM(AP,BP,CP)

*	We now have a "wobbled" normal to simulate surface irregularities

2       RETURN
        END

***********************************************


        SUBROUTINE NORM(A,B,C)
        IMPLICIT REAL(A-H,O-Z)
        F = DSQRT(A*A+B*B+C*C)
        IF(F.EQ.0.0) STOP 50
        A = A/F
        B = B/F
        C = C/F

*	The three real variables are such that A*A + B*B + C*C =1

        RETURN
        END

***********************************************


      SUBROUTINE RFN
      IMPLICIT REAL (A-H,O-Z)
      COMMON/ANGG/ANGLI,ANGLF,LABUT,ANGL,AP,BP,CP
      COMMON/DIR/VX,VY,VZ
      DOTP = AP*VX + BP*VY + CP*VZ
      VX = VX - 2*DOTP*AP
      VY = VY - 2*DOTP*BP
      VZ = VZ - 2*DOTP*CP
      RETURN
      END


***********************************************

* CALCULATES THE DISTANCE (EXDIST) TO THE EXIT OF A REGION (IREG)
* ALONG THE DIRECTION VX,VY,VZ FROM X,Y,Z. THE EXIT SURFACE IS
* RETURNED IN IN ISURF.

      SUBROUTINE DTOEX
      IMPLICIT REAL (A-H,O-Z)
      INTEGER*2 IGEOM(100,600), SURFLAG(600)
      DOUBLE PRECISION EXDIST
      COMMON/DTO/ISURF,EXDIST
      COMMON/DIR/VX,VY,VZ
      COMMON/DEF/X,Y,Z,IREG
      COMMON/GEOM/NSURF,NREG,IGEOM,SURFLAG
      COMMON/GEOM/A(600),B(600),C(600),D(600),E(600),F(600),G(600)
     +,P(600),Q(600),R(600)

        EXDIST=100000.0
        DO 1 I=1,NSURF

	IF(IREG.EQ.13) THEN
	IF(IGEOM(IREG,I).EQ.0.0 .AND. SURFLAG(I).EQ.0.0) GOTO 1
	GOTO 69
        ENDIF

	IF(IGEOM(IREG,I).EQ.0) GOTO 1

69	CONTINUE
	DD=DIST(I)

*	PRINT*,'DD FOR SURFACE',I,'    IS:',DD

        IF(DD.LT.0.0) GOTO 1
        IF(DD.GT.EXDIST) GOTO 1
        ISURF=I

*	PRINT*,'ISURF (RETURNED BY DTOEX):',ISURF

      EXDIST=DD
1	  CONTINUE
      IF(EXDIST.EQ.100000.0) EXDIST=-1.0
      RETURN
      END

***********************************************

      FUNCTION DIST(I)
      IMPLICIT REAL (A-H,O-Z)
      INTEGER*2 IGEOM(100,600), SURFLAG(600)
      COMMON/DIR/VX,VY,VZ
      COMMON/DEF/X,Y,Z,IREG
      COMMON/GEOM/NSURF,NREG,IGEOM,SURFLAG
      COMMON/GEOM/A(600),B(600),C(600),D(600),E(600),F(600),G(600)
     +,P(600),Q(600),R(600)
C CALCULATES THE DIST TO THE I'TH SURFACE FROM (X,Y,Z) IN DIR'N
C (VX,VY,VZ)
      AA=A(I)*VX*VX+C(I)*VY*VY+E(I)*VZ*VZ+P(I)*VX*VY+Q(I)*VY*VZ
     ++R(I)*VX*VZ
      BB=2*(A(I)*VX*X+C(I)*VY*Y+E(I)*VZ*Z)
     ++B(I)*VX+D(I)*VY+F(I)*VZ+P(I)*(X*VY+Y*VX)
     ++Q(I)*(Y*VZ+Z*VY)+R(I)*(X*VZ+Z*VX)
      CC=A(I)*X*X+B(I)*X+C(I)*Y*Y+D(I)*Y+E(I)*Z*Z+F(I)*Z+G(I)
     ++P(I)*X*Y+Q(I)*Y*Z+R(I)*X*Z
C WE NOW HAVE TO SOLVE THE QUADRATIC AA.T.T+BB.T+CC=0
      IF(AA.NE.0.) GOTO 1
      IF(BB.NE.0.) GOTO 2
      DIST=-1.0E4
      RETURN
  2   DIST=-CC/BB
      RETURN
  1   DD=BB*BB-4*AA*CC
      IF(DD.GE.0.) GOTO 3
      DIST=-2.0E4
      RETURN
   3  DIST=(-BB+DSQRT(DD))/(2*AA)
      DISS=(-BB-DSQRT(DD))/(2*AA)
      IF(DISS.GT.0..AND.DIST.GT.0.) GOTO 4
      IF(DISS.LT.0..AND.DIST.LT.0.) GOTO 5
      IF(DIST.GT.0.) RETURN
      GOTO 6
   4  IF(DIST.LT.DISS) RETURN
      GOTO 6
   5  IF(DISS.LT.DIST) RETURN
   6  DIST=DISS
      RETURN
C WE HAVE NOW RETURNED THE SMALLEST +VE DIST - IF THERE IS ONE
      END

***********************************************

      SUBROUTINE BIN(WT,X,Y,WX,WY,A,NX,NY)
      IMPLICIT REAL (A-H,O-Z)
      REAL A(NX,NY)
      IX = (X+WX*0.5)/(WX/NX) + 1
      IY = (Y+WY*0.5)/(WY/NY) + 1
      IF(IX.LT.1.OR.IX.GT.NX) RETURN
      IF(IY.LT.1.OR.IY.GT.NY) RETURN
      A(IX,IY) = A(IX,IY) + WT
      RETURN
      END

***********************************************


      SUBROUTINE ABUTL2
C
C  THIS SUBRTN. SIMULATES THE EFFECTS OF ABUTMENT ERRORS BETWEEN
C  GUIDE GLASS SEGMENTS.
C  IT REQUIRES:   GLENG(IREG)  -  THE LENGTH OF GLASS PIECES IN
C                                  THE IREG REGION
C                 ANGLI        -  THE INCOMMING ANGLE TO THE IDEAL
C                                 SURFACE
C                 ANGLF        -  THE OUTGOING ANGLE
      IMPLICIT REAL (A-H,O-Z)
      REAL G05DAF
      COMMON/GLASS/BETA(100),ABUT(100),CREF(100),GLENG(100),GAMMB(100)
     +,GAMMA(100)
      COMMON/ANGG/ANGLI,ANGLF,LABUT,ANGL,AP,BP,CP
      COMMON/DEF/X,Y,Z,IREG
      COMMON/NOR/PL,PLMN,SIG
      SIG = ABUT(IREG)
      GL = GLENG(IREG)
C PLL = PROB OF LANDING LOSS
      PLL = SIG/(2.5066*GL*ANGLI)
      IF(G05DAF(0.,1.).LT.PLL) GO TO 99
C PTL = PROB. OF TAKE-OFF LOSS
      PTL = SIG/(2.5066*GL*ANGLF)
      IF(G05DAF(0.,1.).LT.PTL) GO TO 99
      RETURN
99  	LABUT = 0
      RETURN
      END


***********************************************


*	Tay(x) calculates the intensity at the moderator for each wavelength.

	double precision function tay(x)

	real ache, mass, boltz, temp, rho
	real thermalwave, thermalwavesq, thermalwavecube
	real c1, c2, c3, c4, arg1, expw1w2, arg2num, arg2, expw4w5 
	real wavsq, expthermal, arg3, c1log
	real w1, w2, w3, w4, w5, phi0, alpha, wav, x
	real phiepi, phimax, phifer, scale, phi
	real phifer1, phifer2, phifer3, scalerho   
	real scale3, scale4, scale5, s4, s5, scalew3, scalew2, s2

	ache = 6.6262d-14
	mass = 1.67495d-27
	boltz = 1.3807d-3

	scale = 3.2d+04
	scalerho = 5.55d+02
	scale3 = 5.d-03
	scalew2 = 5.d+01
	s2 = scalew2
	scalew3 = 1.3d+02
	scale4 = 5.d-02
	s4 = scale4
	scale5 = 7.d-03
	s5 = scale5

	w1= 0.107d+01
	w2= 0.90d+00
	w3= 0.100385d+1
	w4= 0.106554d+1
	w5= 0.952889d+00
        rho = 0.996089d+00
	phi0 = 0.101686d+01
	alpha = 0.117703d+01
	temp = 0.978289d+00

	c1 = (ache*ache)/(2*mass)
	c1log = dlog(c1)
	c2 = c1/boltz
	c3 = dsqrt(1.d+03*c1)
	c4 = dsqrt(c2)
	thermalwavesq = c2/(50.*temp)
	thermalwave = dsqrt(thermalwavesq)
	thermalwavecube = thermalwave**3	

	wav = x

	arg1 = (w1*scale3/c3)*wav - w2*s2
	expw1w2 = dexp(arg1)

	arg2num = w4*s4 - (wav/c3)
	arg2 = arg2num/(w5*s5)
	expw4w5 = dexp(arg2)

	wavsq = wav*wav
	arg3 = -thermalwavesq/wavsq
	expthermal = dexp(arg3)

	phifer1 = c1/wav**3
	phifer2 = 1./(1.+expw1w2)    	
	phifer3 = 1.+ (w3*scalew3)/(1.+expw4w5)

	phifer = 2.*phifer1*phifer2*phifer3
	phiepi = 2.*c1**(1.-alpha)*wav**(2.*alpha-3.)
	phimax = 2.*(rho*scalerho)*(thermalwavesq**2/wav**5)*
     *		 expthermal

	phi = scale*phi0*(phimax+phifer*phiepi)
	tay = phi

	return
	end

��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������
