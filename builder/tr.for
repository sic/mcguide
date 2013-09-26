       	SUBROUTINE TAPER(KNT)

*	Calculation of equation coeffs. for tapered guide

      	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	LOGICAL ANSWER,YES,REFL_ANSWER
	integer*2 IGEOM(100,6000),IREFL(100,6000),SUM(100000)
	INTEGER*2 SURFLAG(10000), NODIV
	COMMON /BEND/SURFLAG,NODIV,INT_COEF,POL_EFF,GAMMB_BEND,ORIENT
      	COMMON /COEF/ A(6000),B(6000),C(6000),D(6000),E(6000)
     $      ,F(6000),G(6000),P(6000),Q(6000),R(6000),
     $       SA,SB,SC,SD,SE,SF,SG,SP,SQ,SR,
     $      NREG,NSURF,SLENG
      	COMMON /PARA/ X1,Y1,THETA,RLENG,RADIUS,HEIGHT,WIDTH,
     $       X2,Y2,THETA2,REFL_ANSWER
*	DIMENSION IGEOM(100,6000),IREFL(100,6000),SUM(100000)
	DATA YES/'Y'/
	COMMON/GEOM/IREFL,NUMBER_OF_REGIONS

*	Calculations

      	X2=X1 + RLENG*DSIN(THETA)
      	Y2=Y1 + RLENG*DCOS(THETA)


      	X3=SLENG*DSIN(THETA)
      	Y3=SLENG*DCOS(THETA)

*	Input width and height of exit of guide

      	WRITE(6,1000)
	READ(3,*)
      	READ(3,*) WEXIT, HEXIT
*      	WRITE(6,*) HEXIT,WEXIT
      	DIFF = WEXIT-WIDTH
      	DROP = HEXIT-HEIGHT

*	Now for the coefficients

*	Entrance plane

      	B(KNT) = WIDTH*DSIN(THETA)
      	D(KNT) = WIDTH*DCOS(THETA)
      	G(KNT) = -WIDTH*(X1*DSIN(THETA) + Y1*DCOS(THETA))

*	Right plane

      	A(KNT+1) = 0.0
      	B(KNT+1) = Y2-Y1 + DIFF/2.0*DSIN(THETA)
      	C(KNT+1) = 0.0
      	D(KNT+1) = X1-X2 + DIFF/2.0*DCOS(THETA)
      	G(KNT+1) = Y1*X2-X1*Y2 +WIDTH/2.0*(X2*DSIN(THETA)+Y2*DCOS(THETA))
     $     	 - WEXIT/2.0*(Y1*DCOS(THETA) + X1*DSIN(THETA))

*	Exit plane

      	B(KNT+2) = WEXIT*DSIN(THETA)
      	D(KNT+2) = WEXIT*DCOS(THETA)
      	G(KNT+2) = -WEXIT*(Y2*DCOS(THETA) + X2*DSIN(THETA))

*	Left plane

        A(KNT+3) = 0.0
        B(KNT+3) = Y2-Y1 - DSIN(THETA)/2.0*DIFF
        C(KNT+3) = 0.0
        D(KNT+3) = X1-X2 - DCOS(THETA)/2.0*DIFF
        G(KNT+3) = Y1*X2-X1*Y2 -WIDTH/2.0*(X2*DSIN(THETA)
     $		 + Y2*DCOS(THETA))
     $     	 + WEXIT/2.0*(Y1*DCOS(THETA) 
     $		 + X1*DSIN(THETA))

      	FACTOR=DROP/(2.0*RLENG)

*	Upper plane


C----- UB(KNT+4)== -FACTOR*DSIN(THETA)
      	D(KNT+4) = -FACTOR*DCOS(THETA)
      	F(KNT+4) = 1.0
      	G(KNT+4) = FACTOR*(X1*DSIN(THETA) + Y1*DCOS(THETA)) - HEIGHT/2.0

*	Lower plane

      	B(KNT+5) = -B(KNT+4)
      	D(KNT+5) = -D(KNT+4)
      	F(KNT+5) = 1.0
      	G(KNT+5) = -G(KNT+4)

        SURFLAG(KNT) = 0
	SURFLAG(KNT+1) = 0
	SURFLAG(KNT+2) = 0
	SURFLAG(KNT+3) = 0
	SURFLAG(KNT+4) = 0
	SURFLAG(KNT+5) = 0
	

      	SA = A(1)
      	SB = B(KNT+2)
      	SC = C(1)
      	SD = D(KNT+2)
      	SE = E(1)
      	SF = F(1)
      	SG = -WEXIT*(Y3*DCOS(THETA)+X3*DSIN(THETA))
      	SP = P(1)
      	SQ = Q(1)
      	SR = R(1)

      	THETA2 = THETA
      	

        IF(REFL_ANSWER.EQ.YES.OR.REFL_ANSWER.EQ.yes) THEN
        DO I = 1,NUMBER_OF_REGIONS
                IREFL(I,KNT) = 0
                IREFL(I,KNT+1) = 1
                IREFL(I,KNT+2) = 0
                IREFL(I,KNT+3) = 1
                IREFL(I,KNT+4) = 1
                IREFL(I,KNT+5) = 1
        ENDDO
        ELSE
        DO I = 1,NUMBER_OF_REGIONS
                IREFL(I,KNT) = 0
                IREFL(I,KNT+1) = 0
                IREFL(I,KNT+2) = 0
                IREFL(I,KNT+3) = 0
                IREFL(I,KNT+4) = 0
                IREFL(I,KNT+5) = 0
        ENDDO
        ENDIF



	RETURN

1000   FORMAT('=====> GUIDE BEING TAPERED',/,
     +     ' HEIGHT OF EXIT, WIDTH OF EXIT?')

      END

