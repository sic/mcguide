       	SUBROUTINE STRAIGHT(KNT)

*	Calc. of coeffs. of eqns. of cuboid

      	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        INTEGER*2 SURFLAG(10000), NODIV
	integer*2 IGEOM(100,6000),IREFL(100,6000),SUM(100000)  
      	LOGICAL ANSWER,YES
	REAL SEPDIV, INT_COEF, GAMMB_BEND, POL_EFF, ORIENT
	REAL HW
	COMMON /BEND/SURFLAG,NODIV,INT_COEF,POL_EFF,GAMMB_BEND
     $              ,ORIENT
      	COMMON /COEF/  A(6000),B(6000),C(6000),D(6000),E(6000)
     $      ,F(6000),G(6000),P(6000),Q(6000),R(6000),SA,SB,SC,
     $      SD,SE,SF,SG,SP,SQ,SR,NREG,NSURF,SLENGTH
      	COMMON /PARA/ X1,Y1,THETA,RLENGTH,RADIUS,HEIGHT,WIDTH,
     $		      X2,Y2,THETA2,REFL_ANSWER
	DATA YES/'Y'/
*	DIMENSION IGEOM(100,6000),IREFL(100,6000),SUM(100000)  
	COMMON/GEOM/IREFL,NUMBER_OF_REGIONS

*	Reading in number of internal surfaces
	READ(3,*)
	READ(3,*) NODIV
*	Reading in reflection coefft of internal surfaces
	READ(3,*)
	READ(3,*) GAMMB_BEND,INT_COEF, POL_EFF,ORIENT

*	This version doesn't take into account the actual
*	orientation that will be physically required.
*	(just rotates about entrance)

*	Modified on 15.10.95 to include glass thickness

*	Calculations

*	Converting ORIENTATION ANGLE into radians.
	ORIENT = ORIENT*0.017453293

	GLASS_WIDTH = 0.02

	THETA = THETA + ORIENT

      	X2 = X1 + RLENGTH*DSIN(THETA)
      	Y2 = Y1 + RLENGTH*DCOS(THETA)

      	X3 = SLENGTH*DSIN(THETA)
      	Y3 = SLENGTH*DCOS(THETA)
	
*	SEPDIV = WIDTH/(NODIV + 1)

	SEPDIV = (WIDTH - (NODIV*GLASS_WIDTH))/(1+NODIV)

*	Entrance

      	B(KNT) = WIDTH*DSIN(THETA)
      	D(KNT) = WIDTH*DCOS(THETA)
      	G(KNT) = -WIDTH*(Y1*DCOS(THETA) + X1*DSIN(THETA))

*	Right Side

      	A(KNT+1) = 0.0
      	B(KNT+1) = Y2-Y1
      	C(KNT+1) = 0.0
      	D(KNT+1) = X1-X2
      	G(KNT+1) = Y1*X2-X1*Y2+WIDTH/2.0*DCOS(THETA)*(Y2-Y1)
     $     	 + WIDTH/2.0*DSIN(THETA)*(X2-X1)

*	Exit

        B(KNT+2) = B(KNT)
        D(KNT+2) = D(KNT)
        G(KNT+2) = -WIDTH*(Y2*DCOS(THETA) + X2*DSIN(THETA))

*	Left side

      	B(KNT+3) = B(KNT+1)
      	A(KNT+3) = 0.0
      	D(KNT+3) = D(KNT+1)
      	C(KNT+3) = 0.0
      	G(KNT+3) = Y1*X2-X1*Y2 + WIDTH/2.0*DCOS(THETA)*(Y1-Y2)
     $           + WIDTH/2.0*DSIN(THETA)*(X1-X2)

*	Top

      	B(KNT+4) = 0.0
      	D(KNT+4) = 0.0
      	F(KNT+4) = 1.0
      	G(KNT+4) = -HEIGHT/2.0

*	Bottom

 	B(KNT+5) = 0.0
      	D(KNT+5) = 0.0
      	F(KNT+5) = 1.0
      	G(KNT+5) = HEIGHT/2.0

	SURFLAG(KNT) = 0
	SURFLAG(KNT+1) = 1
	SURFLAG(KNT+2) = 0
        SURFLAG(KNT+3) = 1
	SURFLAG(KNT+4) = 0
	SURFLAG(KNT+5) = 0
        

	HW = WIDTH/2

	TEMP = 0

	LENGTH1 = SEPDIV

*	Internal surfaces

        DO I = 1, NODIV

	TEMP = TEMP + 1

	A(KNT+5+TEMP)=0.0
	B(KNT+5+TEMP)=B(KNT+1)
	C(KNT+5+TEMP)=0.0
	D(KNT+5+TEMP)=D(KNT+1)
*	G(KNT+5+TEMP)=(((X2-X1)*(Y1+(WIDTH/2.0*DSIN(THETA))
*     $      - (LENGTH1*DSIN(THETA)))) - (((Y2-Y1)*(X1-
*     $      (WIDTH/2.0*DCOS(THETA))+(LENGTH1*DCOS(THETA))))

	G(KNT+5+TEMP)=(X2-X1)*(Y1+(HW*DSIN(THETA))-(LENGTH1*DSIN(THETA)))
     +   -(Y2-Y1)*(X1-(HW*DCOS(THETA))+(LENGTH1*DCOS(THETA)))

        SURFLAG(KNT+5+TEMP) = 2

	TEMP = TEMP + 1
        LENGTH1 = LENGTH1 + GLASS_WIDTH

	A(KNT+5+TEMP)=0.0
	B(KNT+5+TEMP)=B(KNT+1)
	C(KNT+5+TEMP)=0.0
	D(KNT+5+TEMP)=D(KNT+1)
*	G(KNT+5+TEMP)=(((X2-X1)*(Y1+(WIDTH/2.0*DSIN(THETA))
*     $      - (LENGTH1*DSIN(THETA)))) - (((Y2-Y1)*(X1-
*     $      (WIDTH/2.0*DCOS(THETA))+(LENGTH1*DCOS(THETA))))

	G(KNT+5+TEMP)=(X2-X1)*(Y1+(HW*DSIN(THETA))-(LENGTH1*DSIN(THETA)))
     +   -(Y2-Y1)*(X1-(HW*DCOS(THETA))+(LENGTH1*DCOS(THETA)))

        SURFLAG(KNT+5+TEMP) = 1

	LENGTH1 = LENGTH1 + SEPDIV

	ENDDO


	SA = A(1)
      	SB = B(KNT)
      	SC = C(1)
      	SD = D(KNT)
      	SE = E(1)
      	SF = F(1)
      	SG = -WIDTH*(Y3*DCOS(THETA)+X3*DSIN(THETA))
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
                IREFL(I,KNT+4) = 0
                IREFL(I,KNT+5) = 0

                DO J=1,NODIV
                        IREFL(I,KNT+5+J) = 1
                ENDDO
        ENDDO
        ELSE
        DO I = 1,NUMBER_OF_REGIONS
                IREFL(I,KNT) = 0
                IREFL(I,KNT+1) = 0
                IREFL(I,KNT+2) = 0
                IREFL(I,KNT+3) = 0
                IREFL(I,KNT+4) = 0
                IREFL(I,KNT+5) = 0

                DO J=1,NODIV
                        IREFL(I,KNT+5+J) = 0
                ENDDO
        ENDDO
        ENDIF


      	RETURN

273	FORMAT(5F10.6)
      	
	END

