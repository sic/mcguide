       	SUBROUTINE PLANE(KNT)

*	Calc. of coeffs. of eqns. of cuboid

      	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      	LOGICAL ANSWER,YES,REFL_ANSWER
 	integer*2 IGEOM(100,6000),IREFL(100,6000),SUM(100000)
	integer*2 SURFLAG(10000), NODIV		
      	COMMON /COEF/  A(6000),B(6000),C(6000),D(6000),E(6000)
     $      ,F(6000),G(6000),P(6000),Q(6000),R(6000),
     $       SA,SB,SC,SD,SE,SF,SG,SP,SQ,SR,
     $       NREG,NSURF,SLENGTH
	COMMON / BEND/SURFLAG,NODIV,INT_COEF,POL_EFF,GAMMB_BEND,ORIENT
      	COMMON /PARA/ X1,Y1,THETA,RLENGTH,RADIUS,HEIGHT,WIDTH,
     $		      X2,Y2,THETA2,REFL_ANSWER
*        DIMENSION IGEOM(100,6000),IREFL(100,6000),SUM(100000)
	COMMON/GEOM/IREFL,NUMBER_OF_REGIONS
        DATA YES/'Y'/


*	Calculations

      	X2 = X1 + RLENGTH*DSIN(THETA)
      	Y2 = Y1 + RLENGTH*DCOS(THETA)

      	X3 = SLENGTH*DSIN(THETA)
      	Y3 = SLENGTH*DCOS(THETA)

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
	
*	Writing SURFLAG coeffts for each surface.

	SURFLAG(KNT) = 0
	SURFLAG(KNT+1) = 0
	SURFLAG(KNT+2) = 0
	SURFLAG(KNT+3) = 0
	SURFLAG(KNT+4) = 0
	SURFLAG(KNT+5) = 0



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
      	
	END

