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
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIceTmp  ! temporary glacier ice content [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilLiqWaterTmp  ! temporary glacier liquid water content [m3/m3]

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
              SnowfallDensity          => noahmp%water%state%SnowfallDensity           ,& ! in,     bulk density of snowfall (kg/m3)
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! in,     latent heat of vaporization/subli (j/kg), ground
              FGEV            => noahmp%energy%flux%FGEV             ,& ! inout,  glacier evap heat (w/m2) [+ to atm]
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! inout,  actual number of snow layers (negative)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! inout,  thickness of snow/glacier layers (m)
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout,  snow water equivalent [mm]
              SnowWaterEquivPrev          => noahmp%water%state%SnowWaterEquivPrev           ,& ! inout,  snow water equivalent at last time step [mm]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  glacier water content [m3/m3]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! inout,  glacier ice moisture (m3/m3)
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! inout,  total glacier water [m3/m3]
              PondSfcThinSnwMelt         => noahmp%water%state%PondSfcThinSnwMelt          ,& ! inout, surface ponding [mm] from snowmelt when thin snow has no layer
              WaterHeadSfc       => noahmp%water%state%WaterHeadSfc        ,& ! inout,  surface water head (mm) 
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! inout,  water input on glacier/soil surface [mm/s]
              QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! inout,  snow surface frost rate[mm/s]
              QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! inout,  snow surface sublimation rate[mm/s]
              SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! inout,  glacier flow [mm/s]
              SNOWHIN         => noahmp%water%flux%SNOWHIN           ,& ! out,    snow depth increasing rate (m/s)
              EDIR            => noahmp%water%flux%EDIR              ,& ! out,    net direct glacier evaporation (mm/s)
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s]
              QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
              PondSfcThinSnwComb        => noahmp%water%state%PondSfcThinSnwComb         ,& ! out,    surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans        => noahmp%water%state%PondSfcThinSnwTrans          & ! out,   surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

    ! initialize
    allocate( SoilIceTmp(1:NumSoilLayer) )
    allocate( SoilLiqWaterTmp(1:NumSoilLayer) )
    SNOFLOW   = 0.0
    RUNSUB    = 0.0
    RUNSRF    = 0.0
    SNOWHIN   = 0.0

    ! prepare for water process
    SoilIce(:)   = max(0.0, SoilMoisture(:)-SoilLiqWater(:))
    SoilIceTmp = SoilIce
    SoilLiqWaterTmp = SoilLiqWater      
    SnowWaterEquivPrev    = SnowWaterEquiv

    ! compute soil/snow surface evap/dew rate based on energy flux
    QVAP      = max(FGEV/LATHEAG, 0.0)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW      = abs(min(FGEV/LATHEAG, 0.0))  ! negative part of fgev
    EDIR      = QVAP - QDEW

    ! snow height increase
    SNOWHIN = QSNOW / SnowfallDensity

    ! ground sublimation and evaporation
    QSNSUB = QVAP

    ! ground frost and dew
    QSNFRO = QDEW

    ! snowpack water processs
    call SnowWaterMainGlacier(noahmp)

    ! total surface input water to glacier ice
    QINSUR = (PondSfcThinSnwMelt + PondSfcThinSnwComb + PondSfcThinSnwTrans) / MainTimeStep * 0.001  ! convert units (mm/s -> m/s)
    if ( NumSnowLayerNeg == 0 ) then
       QINSUR = QINSUR + (QSNBOT + QRAIN) * 0.001
    else
       QINSUR = QINSUR + QSNBOT * 0.001
    endif
#ifdef WRF_HYDRO
    QINSUR = QINSUR + WaterHeadSfc / MainTimeStep * 0.001
#endif

    ! surface runoff
    RUNSRF = QINSUR * 1000.0   ! mm/s

    ! glacier ice water
    if ( OptGlacierTreatment == 1 ) then
       REPLACE = 0.0
       do ILEV = 1, NumSoilLayer
          REPLACE = REPLACE + ThicknessSnowSoilLayer(ILEV)*(SoilIce(ILEV) - SoilIceTmp(ILEV) + SoilLiqWater(ILEV) - SoilLiqWaterTmp(ILEV))
       enddo
       REPLACE = REPLACE * 1000.0 / MainTimeStep     ! convert to [mm/s]
       SoilIce    = min(1.0, SoilIceTmp)
    elseif ( OptGlacierTreatment == 2 ) then
       SoilIce = 1.0
    endif
    SoilLiqWater = 1.0 - SoilIce

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

    if ( maxval(SoilIce) < 0.0001 ) then
       write(*,*) "GLACIER HAS MELTED AT:",GridIndexI,GridIndexJ," ARE YOU SURE THIS SHOULD BE A GLACIER POINT?"
       !CALL wrf_debug(10,TRIM(message))
    endif

    end associate

  end subroutine WaterMainGlacier

end module WaterMainGlacierMod
