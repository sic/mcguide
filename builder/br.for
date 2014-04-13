        PROGRAM BUILDER   
	
*	Calc. of coeffs of eqns. of regions of a guide.

*	Calcs eqn. coeffs. for surfaces of guide sections.
*	Guide is assumed to extend in "XY" plane, with it's height
*	in the "Z" direction.
*	Each guide section may be rectangular, tapered of circular.

*       User specifies starting point of first guide section
*	and then following sections are added to the exits of
*	the previous section	

*	Program has been modified to include calculations for bender
*	type guides.

      	IMPLICIT  DOUBLE PRECISION(A-H,O-Z)
	CHARACTER*40 NAME
      	INTEGER*2 SURFLAG(10000), NODIV, SURFLAG0
       	integer*2 IGEOM(100,6000),IREFL(100,6000),SUM(100000)
 
	REAL INT_COEF,gammb_bend,pol_eff,ORIENT
      	INTEGER NUMBER_OF_REGIONS
	LOGICAL ANSWER,DEFL_ANSWER,YES,REPEAT,BEND_ANSWER
	LOGICAL REFL_ANSWER
	COMMON/GEOM/IREFL,NUMBER_OF_REGIONS
      	COMMON /COEF/ A(6000),B(6000),C(6000),D(6000),E(6000),
     $       F(6000),G(6000),P(6000),Q(6000),R(6000),SA,SB,SC,
     $       SD,SE,SF,SG,SP,SQ,SR,NREG,NSURF,SEPARATION
        COMMON /BEND/SURFLAG,NODIV,INT_COEF,POL_EFF,GAMMB_BEND
     $		,ORIENT
      	COMMON /PARA/ X1,Y1,THETA,RLENGTH,RADIUS,HEIGHT,WIDTH,
     $ 		      X2,Y2,THETA2,REFL_ANSWER   
      	DATA YES/'Y'/,REPEAT/'R'/
*      	DIMENSION IGEOM(100,6000),IREFL(100,6000),SUM(100000)
 
	DIMENSION WAVINESS(100)
	DIMENSION REF_COEF(100),ABUT(100),GLENG(100),GAMMA_C(100)

        OPEN(UNIT=2,FILE='BR.DAT',STATUS='NEW')
        OPEN(UNIT=3,FILE='BR.IN',STATUS='OLD')        

	open(unit=99,file='coords.dat',status='new')
        

*	Set all initial values to zero

      	NREG = 0
      	NSURF = 0
      	KNT = 1 
      	M = 1
	
      	DO I = 1, 600
	      A(I) = 0.0
	      B(I) = 0.0
	      C(I) = 0.0
	      D(I) = 0.0
	      E(I) = 0.0
	      F(I) = 0.0
	      G(I) = 0.0
	      P(I) = 0.0
	      Q(I) = 0.0
	      R(I) = 0.0
      	ENDDO

	
	DO I = 1, 10000
		SUM(I) = 0
	ENDDO

      	DO I = 1, 100
      		WAVINESS(I) = 0
      		REF_COEF(I) = 0
      		ABUT(I) = 0
      		GLENG(I)= 0
	ENDDO

      	IGEOM(0,0) = 0
	gammb_bend = 0
	pol_eff = 0
	int_coef = 0
	DO I = 1, 100
		DO J = 1, 100
			IREFL(I,J) = 0
		ENDDO
	ENDDO

*	Read starting parameters for first guide
       
*	Reading entrance of guide coordinates

	READ(3,*)
        READ(3,101)NAME
C	print*,name
	READ(3,*)
	READ(3,*)NUMBER_OF_REGIONS
C	print*,number_of_regions
	READ(3,*)
	READ(3,*)
       	READ(3,271)X1,Y1
C	print*,x1,y1

*	Reading direction of guide with respect to "Y" axis (radians)	

	READ(3,*)
       	READ(3,271)THETA
C	print*,theta
	VL=Y1

*	Reading length, width, height and radius of curvature of guide section

	READ(3,*)
	READ(3,*)
      	READ(3,275)RLENGTH,WIDTH,HEIGHT,RADIUS
C        print*,rlength,width,height,radius

	IF (RLENGTH.LT.0.0) THEN
		WRITE(6,1012)
		STOP
	ENDIF
	IF (WIDTH.LT.0.0) THEN 
		WRITE(6,1013)	
                STOP
	ENDIF
	IF (HEIGHT.LT.0.0) THEN
	        WRITE(6,1014)
		STOP
	ENDIF

      	GX = WIDTH
	GY = RLENGTH
      	GZ = HEIGHT

*	Reading number of neutron histories.

	READ(3,*)
      	READ(3,272)NHIST
C	print*,nhist

*	Reading moderator and sample plane dimensions

	READ(3,*)
      	READ(3,271)RMX,RMZ,SX,SZ
C	print*,rmx,rmz,sx,sz

*	Reading number of wavelengths, first wavelength and 
*	wavelength increments

	READ(3,*)
       	READ(3,273)NOWAV,FWAV,WAVINC
C	print*,nowav,fwav,wavinc

*	Reading angle between moderator and "Y" axis (degrees)

	READ(3,*)
	READ(3,271)FI
C	print*,fi


*	Reading angular range over wich neutrons are selected

	READ(3,*)
      	READ(3,271)BEETA
C	print*,beeta

	GOTO 70

*	Repeat following code for each guide section

*	First read in guide specification
65	READ(3,*)
	READ(3,*)
	READ(3,*)
        READ(3,275)RLENGTH,WIDTH,HEIGHT,RADIUS

	IF (RLENGTH.LT.0.0) THEN
		WRITE(6,1012)
		STOP
	ENDIF
	IF (WIDTH.LT.0.0) THEN 
		WRITE(6,1013)	
                STOP
	ENDIF
	IF (HEIGHT.LT.0.0) THEN
	        WRITE(6,1014)                      
		STOP
	ENDIF

* 	Reading separation between guide exit and sample plane

70	READ(3,*)
      	READ(3,271)SEPARATION
C	print*,separation


*	Reading surface waviness, reflection coefficient and 
*	S.D. of abutment error
*	Reading length of glass section within region
*       Reading critical angle per unit of wavelength within region

	READ(3,*)
	READ(3,*)
	READ(3,*)

      	DO I = NREG+1,NREG+1
      		READ(3,271)WAVINESS(I),REF_COEF(I),ABUT(I),
     $                     GLENG(I),GAMMA_C(I)
	ENDDO	


	NODIV = 0
*	INT_COEF = 0

*	Reading if Section is a bender
	READ(3,*)
	READ(3,1035) BEND_ANSWER
C	print*,bend_answer
        

*	Reading in if section is reflective
	READ(3,*)
	READ(3,1036)REFL_ANSWER

	IF(BEND_ANSWER.NE.YES.OR.BEND_ANSWER.NE.yes) GOTO 75

	IF(RADIUS.EQ.0.0) THEN
		CALL STRAIGHT(KNT)
		read(3,*)
		read(3,*)
		read(3,*)
		read(3,*)
		go to 105  
	ELSE
		CALL BENDER(KNT)
		read(3,*)
		read(3,*)
		read(3,*)
		read(3,*)  
		go to 105
	ENDIF


*	Calc. coeffs. of eqns. for either circular, rectangular or
*	tapered guide.

75    	IF (RADIUS.EQ.0.0) GOTO 104

*	If radius not equal 0 => Circular guide
	
	print*,'Calling subroutine CIRC'
      	CALL CIRC(KNT)
      	GOTO 105

*	Reading if the guide is deflexed in the case of a non-circular guide
*	or tapered if it is not deflexed. If it is not deflexed or tapered, it
*	is assumed straight.

104	READ(3,*)
	READ(3,1035) DEFL_ANSWER

	IF (DEFL_ANSWER.EQ.YES.OR.DEFL_ANSWER.EQ.yes)THEN 
		print*,'Calling subroutine DEFLECT'
		CALL DEFLECT(KNT)
		READ(3,*)
		READ(3,*)
		GOTO 105
	ELSE
	        READ(3,*) 
		READ(3,1035) ANSWER
		IF (ANSWER.EQ.YES.OR.ANSWER.EQ.yes) CALL TAPER(KNT)
      		IF (ANSWER.NE.YES.OR.ANSWER.NE.yes) CALL PLANE(KNT)
	ENDIF

*	Reading if there is another section, stop or repeat the section
*	just completed


	write(99,*)'x1,y1,x2,y2 for region ',nreg
	write(99,*) x1, y1
	write(99,*) x2, y2


105	READ(3,*)
        READ(3,1035) ANSWER

        PRINT*,'KNT=',KNT
	
      	IF (ANSWER.EQ.REPEAT) GOTO 65
      	M = M + 6 + 2*NODIV
      	NREG = NREG + 1
      	TOTSURF = NSURF
	NSURF = NSURF + 6 + 2*NODIV
      	X = X1
      	Z = 0.
      	Y = Y1 + 0.1

	DO I = NREG, NREG
		DO J = 1, NSURF
			IGEOM(I,J) = 0
		ENDDO
	ENDDO

	IGM0 = 0

	WRITE(6,2500)NREG 

      	DO I = NREG, NREG
      		 
	print*,'TOTSURF:',TOTSURF,'   NSURF:',NSURF

		DO J = TOTSURF+1,NSURF
*		DO J = (6*(I-1)) + 1,6*I
                     if(nreg.eq.15) then
			print*,'surface:',j
		     endif
      			SUM(J) = A(J)*X*X + B(J)*X
     $	        	       + C(J)*Y*Y + D(J)*Y
     $			       + E(J)*Z*Z + F(J)*Z
     $			       + G(J)
     $			       + P(J)*X*Y + Q(J)*Y*Z + R(J)*Z*X
      			IF (SUM(J).LT.0) THEN
        			IGEOM(I,J) = -1
      			ELSE
          			IGEOM(I,J) = 1
      			ENDIF
			CONTINUE


*		IF (SURFLAG(J).GT.0) THEN
*				IGEOM(I,J) = 0
*		ENDIF

		IF (J.GT.TOTSURF+6) THEN
			IGEOM(I,J) = 0
		ENDIF

		print*,igeom(i,j)
		
	
	

		CONTINUE	
	        ENDDO
        ENDDO

      	KNT = KNT + 6 + 2*NODIV

      	IF (ANSWER.EQ.YES) GOTO 98
      	IF (ANSWER.NE.YES) GOTO 99

*	Another: so set entrance of next section to exit of previous one

98  	X1 = X2
      	Y1 = Y2
      	THETA = THETA2
      	GOTO 65

*	Calculating origin with respect to a coordinate system located at 
*	guide exit.

99      X0 =  Y2*DSIN(THETA2)-X2*DCOS(THETA2)
      	Y0 = -X2*DSIN(THETA2)-Y2*DCOS(THETA2)

      	ISURF = NSURF+1

*	Writing number of neutron histories, number of surfaces and number of
*	regions.
	WRITE(2,101)NAME
	WRITE(2,*)
	WRITE(2,1069) 	
      	WRITE(2,1070) NHIST, ISURF, NREG
	WRITE(2,*)
*	Writing coefficients of the first surface: guide entrance.
	WRITE(2,1109) 
      	WRITE(2,1111) A(1), B(1), C(1), D(1), E(1)
      	WRITE(2,1111) F(1), G(1), P(1), Q(1), R(1)
	WRITE(2,*)
*	Writing coefficients of the second surface: sample plane in a
*	coordinate system located at the guide exit.
	WRITE(2,1110)
      	WRITE(2,1111) SA, SB, SC, SD, SE
	WRITE(2,1111) SF, SG, SP, SQ, SR
	WRITE(2,*)

*	Writing coefficients of the rest of the surfaces.

      	DO N = 2, NSURF
		WRITE(2,*)
		WRITE(2,1112) N
	      	WRITE(2,1111) A(N), B(N), C(N), D(N), E(N)
      		WRITE(2,1111) F(N), G(N), P(N), Q(N), R(N) 
		WRITE(2,*)
	ENDDO                
        WRITE(2,*)
                     
*	Writing the "igeom" of the first surface, then the "igeom" of the
*	sample plane surface (igm0 = 0) and last the "igeom" of the rest of 
*	the surfaces, everything for each region. 

	WRITE(2,1023) 
      	DO I = 1, NREG
		WRITE(2,1024) I
		WRITE(2,1026) IGEOM(I,1), IGM0, 
     $			      (IGEOM(I,J), J = 2, NSURF)
		WRITE(2,*)
	ENDDO

*	Writing the "irefl" of the first surface, then the "irefl" of the
*	sample plane surface (irefl0 = 0) and last the "igeom" of the re 
*    of	the surfaces, everything for each region. 

       	IREFL0 = 0

	WRITE(2,1027) 
      	DO  I = 1, NREG
		WRITE(2,1028) I
		WRITE(2,1026) IREFL(I,1), IREFL0, 
     $			      (IREFL(I,J), J = 2, NSURF)
		WRITE(2,*)
	ENDDO
	
*	Writing "y" coordinate of the guide entrance, guide dimensions,
*	moderator dimensions and sample dimensions.

	WRITE(2,1019)
	WRITE(2,1020) VL,GX,GZ
	WRITE(2,*)
	WRITE(2,1017)
	WRITE(2,1016) RMX,RMZ
	WRITE(2,*)
	WRITE(2,1015)
	WRITE(2,1016) SX,SZ
	WRITE(2,*)

*	Writing angle with respect to "Y-axis" coordinate and the "x" and
*	"y" coordinates of the guide exit.  

	WRITE(2,1021) 
      	WRITE(2,1030) THETA2,X2,Y2
      	WRITE(*,1030) THETA2,X2,Y2
	WRITE(2,*)

*	Writing number of desired wavelengths, first wavelength and 
*	wavelength increment.

	WRITE(2,1022) 
	WRITE(2,1095) NOWAV, FWAV, WAVINC
	WRITE(2,*)

*	Writing guide waviness, reflection coefficient, standar deviation of
*	the abutment error, length of region and critical angle for each region.

	WRITE(2,2003)
      	DO  I = 1, NREG
		WRITE(2,2004) I
	      	WRITE(2,2005) WAVINESS(I),REF_COEF(I),ABUT(I),
     $ 	          	      GLENG(I),GAMMA_C(I)
		WRITE(2,*)
	ENDDO

*	Writing angle between moderator and the "Y-axis".

	WRITE(2,2018)
      	WRITE(2,2020) FI
	WRITE(2,*)

*	Writing angular range within neutrons are selected.
	
        WRITE(2,2019)
      	WRITE(2,2020) BEETA     
	WRITE(2,*)

*	Writing the Surflag coefficients
        SURFLAG0 = 0.0
*	DO I = 1,NSURF
*	SURFLAG(I) = 1
*        ENDDO

	WRITE(2,1099) 
	WRITE(2,1026) SURFLAG(1), SURFLAG0,
     +                        (SURFLAG(J), J = 2, NSURF)
	WRITE(2,*)

*	Writing the Reflection coefft of the internal surfaces

	WRITE(2,1098)	
*	WRITE(2,276) gammb_bend,INT_COEF,pol_eff
	WRITE(2,*) gammb_bend,INT_COEF,pol_eff

*	WRITE(2,*) gammb_bend,1,100
 
        WRITE(2,*)

101	FORMAT(A40)
271   	FORMAT(5F10.4)
272   	FORMAT(2I10)
273   	FORMAT(I10,2F10.4)
275   	FORMAT(5F10.2)
276	FORMAT(5F10.6)

1012	FORMAT('=====> Your guide length has a negative value!',/,
     $          7x,'Please replace it in br.in',/,80x)
1013	FORMAT('=====> Your guide width has a negative value!',/,
     $          7x,'Please replace it in br.in',/,80x)
1014	FORMAT('=====> Your guide height has a negative value!',/,
     $          7x,'Please replace it in br.in',/,80x)
1055	FORMAT('=====> Your guide radius of curvature is negative!',/,
     $          7x,'Please replace it in br.in',/,80x)
1015	FORMAT('Sample dimensions: width and height')
1016 	FORMAT(2F10.3)
1017	FORMAT('Moderator dimensions: width and height')
1019	FORMAT('"Y" coordinate of the guide entrance and',
     $	       ' guide dimensions: ',/,'width and height')
1020 	FORMAT(3F10.3)
1030 	FORMAT(D18.11,2F14.7)
1021	FORMAT('Angle with respect to "Y" axis, x coordinate and',
     $         ' y coordinate',/,'of the exit of the guide')
1022	FORMAT('Number of selected wavelengths, first wavelength and', 
     $	       /,'wavelength increment for region:',I3)
1109	FORMAT('Coefficients of the first surface, guide entrance,',
     $	       'in this order: ',/,
     $	       'X^2, X, Y^2, Y, Z^2, Z, INDEPENDENT, X*Y, Y*Z, Z*X')
1110	FORMAT('Coefficients of the second surface, sample plane, with',
     $	       ' respect to a ',/,'coordinate system located at the',
     $	       ' guide exit.')
1111 	FORMAT(5E15.6)
1112	FORMAT('Coefficients of surface',I3)
1023	FORMAT('The second value is "igeom" for the sample plane surface', 
     $	       ' and equals zero.')
1024	FORMAT('"Igeom" values in region:',I3,'.')
1026 	FORMAT(40I2)
1027	FORMAT('Reflecting character of each surface within a region:',
     $          /,'0 non-reflecting surface, 1 reflecting surface.',/,
     $         'Viewing from the top: guide section entrance,',
     $         ' left hand side',/,
     $         'guide section exit, right hand side, top and',
     $         ' bottom.',/,'The second value is "irefl" for the sample',
     $	       ' plane surface and equals zero.')   
1028	FORMAT('"Irefl" values in region:',I3,'.')
1029	FORMAT(40I2)
1035 	FORMAT(A1)
1036	FORMAT(A)
1037 	FORMAT(1H ,'=====> REPEATING SECTION'/80X)
1040 	FORMAT(' X2 =',D15.8,/,' Y2 =',D15.8)
1042 	FORMAT(' THETA =',D15.8,' RADIANS.')
1045 	FORMAT('=====> READING IF THERE IS ANY MORE SECTIONS'/80X)
1050 	FORMAT(///' ORIGIN IS AT (',D13.5,',',D13.5,') WITH RESPECT TO',
     $ 	/,' CO-ORDINATE SYSTEM DEFINED BY X2,Y2,THETA.')
1060 	FORMAT('=====> READING IF THIS IS A TAPERED GUIDE'/80X)
1069	FORMAT('Number of neutrons, number of surfaces and', 
     $	       ' number of regions.')
1070 	FORMAT(3I10)
1075 	FORMAT('=====> READING IF THIS IS A REFLECTING REGION: 
     $ 	(Y/N)'/80X)
1085 	FORMAT('=====> READING MODERATOR AND SAMPLE PLANE DIMENSIONS:'/
     $  7X,' (MX,MZ,SX,SZ)'/80X)
1090 	FORMAT('=====> READING NUMBER OF WAVELENGTHS,FIRST WAVELENGTH'/
     $  7X,'AND WAVELENGTH INCREMENTS'/80X)
1095 	FORMAT(I10,2F10.2)
1098	FORMAT('Critical angle per unit wavelength, Reflection Coeff,',
     $         'Polarizing efficiency.')
1099	FORMAT('Surface Flag = 1 for internal surface')
2000 	FORMAT('=====> READING SURFACE WAVINESS,REFLECTION COEFF. AND'/ 
     $  7X,'S.D. OF ABUTMENT ERROR'/80X)	
2003	FORMAT('Surface waviness, reflection coefficient,',
     $         ' standard deviation of abutment',/, 
     $         'error, section length and critical angle in')
2004	FORMAT('Region: ',I3)
2005 	FORMAT(5F15.6)
2010 	FORMAT('=====> READING THE LENGTH OF THE GLASS',
     $  1X,'SECTION WITHIN REGION'/80X)
2011 	FORMAT('=====> READING THE CRITICAL ANGLE/UNIT WAVELENGTH',
     $ 	1X,'WITHIN REGION'/80X)
2015 	FORMAT('=====> READING ANGLE BETWEEN MODERATOR AND "Y" AXIS'/ 
     $  7X,'(DEGREES)'/80X)
2018	FORMAT('Angle between moderator and "Y-axis" (degrees).')
2019	FORMAT('Angular range over which neutrons are selected',/,
     $	' (times the critical angle).')
2020 	FORMAT(F10.3)
2025 	FORMAT('=====> READING ANGULAR RANGE OVER WHICH NEUTRONS
     $	ARE SELECTED: (BEETA)'/
     $  7X,'(IF ONLY NEUTRONS WITH ANGLE < GAMMA-C ARE REQUIRED'/
     $  7X,'PUT BEETA = 1)'/80X)
2030 	FORMAT(F5.2)
2500 	FORMAT(80x/'                     ------------------------'/
     $  '                    | GOING FOR SECTION #',I3,' |',/,
     $  '                     ------------------------'/80X/80X)     
2501 	FORMAT('=====     END OF PROGRAM     =====')     

      	CLOSE(2)
      	CLOSE(3)
        close(99)
      	STOP
      	END
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   