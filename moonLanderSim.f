
      PROGRAM MOONLANDER
C SIMPLE MOON LANDER SIMULATION PROGRAM
C    THIS WILL USE EUATIONS OF MOTION TO GIVE Y POSITION UPDATES
C    BASED ON INITIAL CONDITIONS AND USER INPUT OF BURN IN Y DIRECTION
C    
C     ASSUMUPTIONS AND DETAILS: 
C
C     INITIALLY ONLY CONSIDERING Y AXIS +VE UP -VE DOWN
C     ACCELERATION DUE TO ENGINE IS +VE IN UP DIRECTION
C     NEWTONIAN MECHANICS
C     TIME UPDATES (T) ARE IN 0.1 SECOND INCREMENTS
C     BURN TIME IS FOR PILOT SELECTED DURATION
C     BURN % IS PILOT SELECTED
C     BURN WILL RUN AFTER BEING SET TIL DURATION COMPLETE
C     POSITION AND OTHER DATA WILL BE DISPLAYED IN 0.1SECOND INCS
C     MASSES OF LANDER TAKEN FROM WIKIPEDIA DRY 4280KG FUEL 2353KG
C     APS MAIN ENGINE AT 100% = 16,000 NEWTONS
C     UNITS ARE SI, MASS=KG, DURATION=SECONDS, LENGTHS=METERS
C     V = V0 + AT
C     X(T)= ((V + V0) / 2) * T
C     X(T) = V0*T + ((A*T**2) / 2)
C     F=MA -->> A = F/M

C     DECLARE VARIABLES
      DOUBLEPRECISION FMASS, VMASS, VMASS_DRY, FMASS_INITIAL
C     BURN FACTOR IS THE AMOUNT OF MASS OF FUEL (IN KG) BURNED PER SECOND
C     PER PERCENTAGE THRUST      
      DOUBLEPRECISION BURNFACTOR, BURNDUR, BURNPERCENT
      DOUBLEPRECISION AY, GRAV
      DOUBLEPRECISION T, DT
      DOUBLEPRECISION DY, Y0, Y_ALT
      DOUBLEPRECISION T_MAIN,M_ENG_MAXT      
      DOUBLEPRECISION V0, DV ,V
      INTEGER RUNAUTO
      
      

C     SETUP INITIAL CONDITIONS AND CONSTANTS      
      GRAV= -1.625      
      Y0=1000.0
      Y_ALT=Y0
      V=0
      V0=0
      DV=0
      DT=0.1
      T=0
      BURNDUR=0
      BURNPERCENT=0      
      AY = GRAV 
      FMASS_INITIAL = 2353
      FMASS = FMASS_INITIAL
      BURNFACTOR=0.1      
      M_ENG_MAXT=16000
      VMASS_DRY=4280
      VMASS=VMASS_DRY+FMASS
      OPEN (unit=48, file='AltitudeTimeData.csv')
      OPEN (unit=49, file='FuelTimeData.csv')
      OPEN (unit=50, file='ThrustTimeData.csv')

      PRINT *,'INITIAL CONDITIONS'
      PRINT *,'=================='
      PRINT *,'TIME  =',T,     'ALT  =',Y_ALT, 'VELOCITY=',V
      PRINT *,'T_MAIN=',T_MAIN,'FMASS=',FMASS, 'VMASS   =',VMASS
      PRINT *,'VERTICAL ACCELERATION=',AY
      PRINT *,'=================='
      
52    WRITE(*,*)'ENTER THE MAIN ENGINE % THRUST(VERTICAL)'
      READ (*,*)BURNPERCENT
      WRITE(*,*)'ENTER THE BURN DURATION (SEC) INCLUDING FRACTIONS OF'
      READ (*,*)BURNDUR

      IF (BURNPERCENT .EQ. -1) THEN
          RUNAUTO=1
          BURNDUR=0.1
          BURNPERCENT=0
      END IF
C     CHECK IF WE HAVE ENOUGH FUEL, IF LESS THAN ZERO SET T_MAIN=0
56    IF ((FMASS-(BURNFACTOR * BURNPERCENT*DT)) .LT. 0.05) THEN
         T_MAIN = 0.0
         F_MASS = 0.0
      ELSE
         IF (RUNAUTO .NE. 1) THEN
            T_MAIN=(BURNPERCENT / 100.0)*(M_ENG_MAXT)
            FMASS=FMASS - (BURNFACTOR * BURNPERCENT*DT)
         ELSE
            IF (Y_ALT .LT. 700) THEN
              IF (V .GT. 0.1) THEN 
                BURNPERCENT=0.0
                BURNDUR=0.2
              ELSE IF (V .LT. -10.0) THEN
                BURNPERCENT=100
                BURNDUR=0.2
              ELSE IF (V .LT. -5.0) THEN
                BURNPERCENT=80
                BURNDUR=0.2
              ELSE IF (V .LT. -2.5) THEN
                BURNPERCENT=77.0
                BURNDUR=0.2
              ELSE IF (V .LT. -1.5) THEN
                BURNPERCENT=75.0
                BURNDUR=0.2
              ELSE IF (V .LT. -1.0) THEN
                BURNPERCENT=70.0
                BURNDUR=0.2
              ELSE IF (V .LT. -0.5) THEN
                BURNPERCENT=58.0
                BURNDUR=0.2
              ELSE IF (V .LT. -0.2) THEN
                BURNPERCENT=35.0
                BURNDUR=0.2
              END IF 
            END IF 
            T_MAIN=(BURNPERCENT / 100.0)*(M_ENG_MAXT)
            FMASS=FMASS - (BURNFACTOR * BURNPERCENT*DT)
         END IF
      END IF

      AY =  (T_MAIN / VMASS) + GRAV
      
      VMASS=4280+FMASS

      
      DV     = AY * DT      
      V      = V + DV
      DY     = (V / 2.0) * DT
      Y_ALT  = Y_ALT + DY
      T      = T+DT
      BURNDUR = BURNDUR - DT
      PRINT *,' '
      PRINT *,'TIME  =',T,     'ALT  =',Y_ALT, 'VELOCITY=',V
      PRINT *,'T_MAIN=',T_MAIN,'FMASS=',FMASS, 'VMASS   =',VMASS
      PRINT *,'VERTICAL ACCELERATION=',AY
      
      WRITE(48,*) T,Y_ALT
      WRITE(49,*) T,FMASS
      WRITE(50,*) T,T_MAIN

      IF (Y_ALT .LT. 0.0) GOTO 136

      IF (BURNDUR .GT. 0.001) GOTO 56
      
      IF (Y_ALT .GT. 0.0) THEN
          IF (RUNAUTO .NE. 1) THEN 
             GOTO 52   
          ELSE
C             CALL SLEEP(1)
             GOTO 56
          END IF
      END IF   
136   IF (V .LT. -1.0) THEN
          PRINT *,'######## HEAVY LANDING CRASH #########'      
      ELSE
          PRINT *,'*********** GOOD LANDING *************'
      END IF 
      PRINT *,' '
      PRINT *,'FINAL LANDING VALUES'
      PRINT *,'===================='
      PRINT *,'LANDING TIME',T
      PRINT *,'FINAL VERTICAL VELOCITY',V      
      PRINT *,'FUELMASS (KG) REMAINING',FMASS
      PRINT *,'FUELMASS % REMAINING',((FMASS / FMASS_INITIAL)*100)
      PRINT *,'END SIMULATION' 
      
      CLOSE(48)
      CLOSE(49);
      CLOSE(50);
      CALL SYSTEM('gnuplot -p AltitudeTimeData.plt')
      CALL SYSTEM('gnuplot -p FuelTimeData.plt')
      CALL SYSTEM('gnuplot -p ThrustTimeData.plt')
      END
