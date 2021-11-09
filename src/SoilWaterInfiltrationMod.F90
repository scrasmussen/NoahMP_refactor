module SoilWaterInfiltrationMod

!!! Calculate inflitration rate at soil surface and surface runoff

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilWaterInfiltration(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: INFIL
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: K           ! do-loop index
    integer, parameter     :: NFINE = 100 ! no. of fine soil layers of 6m soil
    real(kind=kind_noahmp) :: TEMP        ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZFINE ! layer-bottom depth of the 100-L soil layers to 6.0 m
    allocate( ZFINE(1:NFINE) ) ; ZFINE(:) = 0.0

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]

              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              ZWT             => noahmp%water%state%ZWT               & ! out,    water table depth [m]
             )
! ----------------------------------------------------------------------

! inputs
  REAL,                     INTENT(IN) :: SICEMAX!maximum soil ice content (m3/m3)

! outputs
  REAL,                    INTENT(OUT) :: RUNSRF !surface runoff [mm/s] 
  REAL,                    INTENT(OUT) :: PDDUM  !infiltration rate at surface

! locals
  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL, DIMENSION(1:NSOIL) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3
! --------------------------------------------------------------------------------

    IF (QINSUR >  0.0) THEN
       DT1 = DT /86400.0
       SMCAV = parameters%SMCMAX(1) - parameters%SMCWLT(1)

! maximum infiltration rate

       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - parameters%SMCWLT(1))/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - parameters%SMCWLT(K))/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1.0 - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.0,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.0
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.0
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1.0 - EXP (-ACRT) * SUM
       END IF

! correction of infiltration limitation

       INFMAX = INFMAX * FCR

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND,SH2O(1),SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0.0, QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL



    end associate

  end subroutine SoilWaterInfiltration

end module SoilWaterInfiltrationMod
