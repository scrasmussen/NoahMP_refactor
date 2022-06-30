module WaterMainGlacierMod

!!! Main glacier water module including all water relevant processes
!!! snowpack water -> ice water -> runoff

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowWaterMainGlacierMod, only : SnowWaterMainGlacier

  implicit none

contains

  subroutine WaterMainGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: WATER_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ, ILEV   ! loop index
    real(kind=kind_noahmp) :: REPLACE    ! replacement water due to sublimation of glacier
    real(kind=kind_noahmp), allocatable, dimension(:) :: SICE_SAVE  ! glacier ice content [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SH2O_SAVE  ! glacier liquid water content [m3/m3]

! --------------------------------------------------------------------
    associate(                                                        &
              OptGlacierTreatment => noahmp%config%nmlist%OptGlacierTreatment ,& ! in,    option for glacier treatment
              NumSoilLayer        => noahmp%config%domain%NumSoilLayer        ,& ! in,    number of soil layers
              MainTimeStep        => noahmp%config%domain%MainTimeStep        ,& ! in,    noahmp main time step [s]
              GridIndexI          => noahmp%config%domain%GridIndexI          ,& ! in,    grid index in x-direction
              GridIndexJ          => noahmp%config%domain%GridIndexJ          ,& ! in,    grid index in y-direction
              QVAP            => noahmp%water%flux%QVAP              ,& ! in,     soil surface evaporation rate[mm/s]
              QDEW            => noahmp%water%flux%QDEW              ,& ! in,     soil surface dew rate[mm/s]
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
              QSNOW           => noahmp%water%flux%QSNOW             ,& ! in,     snowfall at ground surface (mm/s)
              BDFALL          => noahmp%water%state%BDFALL           ,& ! in,     bulk density of snowfall (kg/m3)
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! in,     latent heat of vaporization/subli (j/kg), ground
              FGEV            => noahmp%energy%flux%FGEV             ,& ! inout,  glacier evap heat (w/m2) [+ to atm]
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! inout,  actual number of snow layers (negative)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/glacier layers (m)
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
              SNEQVO          => noahmp%water%state%SNEQVO           ,& ! inout,  snow mass at last time step(mm)
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  glacier water content [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! inout,  glacier ice moisture (m3/m3)
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total glacier water [m3/m3]
              PONDING         => noahmp%water%state%PONDING          ,& ! inout,  melting water from snow when there is no layer (mm)
              sfcheadrt       => noahmp%water%state%sfcheadrt        ,& ! inout,  surface water head (mm) 
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! inout,  water input on glacier/soil surface [mm/s]
              QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! inout,  snow surface frost rate[mm/s]
              QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! inout,  snow surface sublimation rate[mm/s]
              SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! inout,  glacier flow [mm/s]
              SNOWHIN         => noahmp%water%flux%SNOWHIN           ,& ! out,    snow depth increasing rate (m/s)
              EDIR            => noahmp%water%flux%EDIR              ,& ! out,    net direct glacier evaporation (mm/s)
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s]
              QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
              PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
              PONDING2        => noahmp%water%state%PONDING2          & ! out,    surface ponding 2 (mm)
             )
! ----------------------------------------------------------------------

    ! initialize
    allocate( SICE_SAVE(1:NumSoilLayer) )
    allocate( SH2O_SAVE(1:NumSoilLayer) )
    SNOFLOW   = 0.0
    RUNSUB    = 0.0
    RUNSRF    = 0.0
    SNOWHIN   = 0.0

    ! prepare for water process
    SICE(:)   = max(0.0, SMC(:)-SH2O(:))
    SICE_SAVE = SICE
    SH2O_SAVE = SH2O      
    SNEQVO    = SNEQV

    ! compute soil/snow surface evap/dew rate based on energy flux
    QVAP      = max(FGEV/LATHEAG, 0.0)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW      = abs(min(FGEV/LATHEAG, 0.0))  ! negative part of fgev
    EDIR      = QVAP - QDEW

    ! snow height increase
    SNOWHIN = QSNOW / BDFALL

    ! ground sublimation and evaporation
    QSNSUB = QVAP

    ! ground frost and dew
    QSNFRO = QDEW

    ! snowpack water processs
    call SnowWaterMainGlacier(noahmp)

    ! total surface input water to glacier ice
    QINSUR = (PONDING + PONDING1 + PONDING2) / MainTimeStep * 0.001  ! convert units (mm/s -> m/s)
    if ( NumSnowLayerNeg == 0 ) then
       QINSUR = QINSUR + (QSNBOT + QRAIN) * 0.001
    else
       QINSUR = QINSUR + QSNBOT * 0.001
    endif
#ifdef WRF_HYDRO
    QINSUR = QINSUR + sfcheadrt / MainTimeStep * 0.001
#endif

    ! surface runoff
    RUNSRF = QINSUR * 1000.0   ! mm/s

    ! glacier ice water
    if ( OptGlacierTreatment == 1 ) then
       REPLACE = 0.0
       do ILEV = 1, NumSoilLayer
          REPLACE = REPLACE + DZSNSO(ILEV)*(SICE(ILEV) - SICE_SAVE(ILEV) + SH2O(ILEV) - SH2O_SAVE(ILEV))
       enddo
       REPLACE = REPLACE * 1000.0 / MainTimeStep     ! convert to [mm/s]
       SICE    = min(1.0, SICE_SAVE)
    elseif ( OptGlacierTreatment == 2 ) then
       SICE = 1.0
    endif
    SH2O = 1.0 - SICE

    ! use RUNSUB as a water balancer, SNOFLOW is snow that disappears, REPLACE is
    ! water from below that replaces glacier loss
    if ( OptGlacierTreatment == 1 ) then
       RUNSUB = SNOFLOW + REPLACE
    elseif ( OptGlacierTreatment == 2 ) then
       RUNSUB = SNOFLOW
       QVAP   = QSNSUB
       QDEW   = QSNFRO
    endif

    if ( OptGlacierTreatment == 2 ) then
       EDIR = QVAP - QDEW
       FGEV = EDIR * LATHEAG
    endif

    if ( maxval(SICE) < 0.0001 ) then
       write(*,*) "GLACIER HAS MELTED AT:",GridIndexI,GridIndexJ," ARE YOU SURE THIS SHOULD BE A GLACIER POINT?"
       !CALL wrf_debug(10,TRIM(message))
    endif

    end associate

  end subroutine WaterMainGlacier

end module WaterMainGlacierMod
