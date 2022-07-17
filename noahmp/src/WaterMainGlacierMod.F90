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
              VaporizeGrd            => noahmp%water%flux%VaporizeGrd              ,& ! in,     ground vaporize rate total (evap+sublim) [mm/s]
              CondenseVapGrd            => noahmp%water%flux%CondenseVapGrd              ,& ! in,     ground vapor condense rate total (dew+frost) [mm/s]
              RainfallGround           => noahmp%water%flux%RainfallGround             ,& ! in,     ground surface rain rate[mm/s]
              SnowfallGround           => noahmp%water%flux%SnowfallGround             ,& ! in,     snowfall on the ground [mm/s]
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
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow            ,& ! inout,  water input on glacier/soil surface [mm/s]
              FrostSnowSfcIce          => noahmp%water%flux%FrostSnowSfcIce            ,& ! inout,  snow surface frost rate[mm/s]
              SublimSnowSfcIce          => noahmp%water%flux%SublimSnowSfcIce            ,& ! inout,  snow surface sublimation rate[mm/s]
              GlacierExcessFlow         => noahmp%water%flux%GlacierExcessFlow           ,& ! inout,  glacier snow excess flow [mm/s]
              SnowDepthIncr         => noahmp%water%flux%SnowDepthIncr           ,& ! out,    snow depth increasing rate (m/s) due to snowfall
              EvapSoilNet            => noahmp%water%flux%EvapSoilNet              ,& ! out,    net direct glacier evaporation (mm/s)
              RunoffSurface          => noahmp%water%flux%RunoffSurface            ,& ! out,    surface runoff [mm/s]
              RunoffSubsurface          => noahmp%water%flux%RunoffSubsurface            ,& ! out,    subsurface runoff [mm/s]
              SnowBotOutflow          => noahmp%water%flux%SnowBotOutflow            ,& ! out,   total water (snowmelt + rain through pack) out of snowpack bottom [mm/s]
              PondSfcThinSnwComb        => noahmp%water%state%PondSfcThinSnwComb         ,& ! out,    surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans        => noahmp%water%state%PondSfcThinSnwTrans          & ! out,   surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

    ! initialize
    allocate( SoilIceTmp(1:NumSoilLayer) )
    allocate( SoilLiqWaterTmp(1:NumSoilLayer) )
    GlacierExcessFlow   = 0.0
    RunoffSubsurface    = 0.0
    RunoffSurface    = 0.0
    SnowDepthIncr   = 0.0

    ! prepare for water process
    SoilIce(:)   = max(0.0, SoilMoisture(:)-SoilLiqWater(:))
    SoilIceTmp = SoilIce
    SoilLiqWaterTmp = SoilLiqWater      
    SnowWaterEquivPrev    = SnowWaterEquiv

    ! compute soil/snow surface evap/dew rate based on energy flux
    VaporizeGrd      = max(FGEV/LATHEAG, 0.0)       ! positive part of fgev; Barlage change to ground v3.6
    CondenseVapGrd      = abs(min(FGEV/LATHEAG, 0.0))  ! negative part of fgev
    EvapSoilNet      = VaporizeGrd - CondenseVapGrd

    ! snow height increase
    SnowDepthIncr = SnowfallGround / SnowfallDensity

    ! ground sublimation and evaporation
    SublimSnowSfcIce = VaporizeGrd

    ! ground frost and dew
    FrostSnowSfcIce = CondenseVapGrd

    ! snowpack water processs
    call SnowWaterMainGlacier(noahmp)

    ! total surface input water to glacier ice
    SoilSfcInflow = (PondSfcThinSnwMelt + PondSfcThinSnwComb + PondSfcThinSnwTrans) / MainTimeStep * 0.001  ! convert units (mm/s -> m/s)
    if ( NumSnowLayerNeg == 0 ) then
       SoilSfcInflow = SoilSfcInflow + (SnowBotOutflow + RainfallGround) * 0.001
    else
       SoilSfcInflow = SoilSfcInflow + SnowBotOutflow * 0.001
    endif
#ifdef WRF_HYDRO
    SoilSfcInflow = SoilSfcInflow + WaterHeadSfc / MainTimeStep * 0.001
#endif

    ! surface runoff
    RunoffSurface = SoilSfcInflow * 1000.0   ! mm/s

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

    ! use RunoffSubsurface as a water balancer, GlacierExcessFlow is snow that disappears, REPLACE is
    ! water from below that replaces glacier loss
    if ( OptGlacierTreatment == 1 ) then
       RunoffSubsurface = GlacierExcessFlow + REPLACE
    elseif ( OptGlacierTreatment == 2 ) then
       RunoffSubsurface = GlacierExcessFlow
       VaporizeGrd   = SublimSnowSfcIce
       CondenseVapGrd   = FrostSnowSfcIce
    endif

    if ( OptGlacierTreatment == 2 ) then
       EvapSoilNet = VaporizeGrd - CondenseVapGrd
       FGEV = EvapSoilNet * LATHEAG
    endif

    if ( maxval(SoilIce) < 0.0001 ) then
       write(*,*) "GLACIER HAS MELTED AT:",GridIndexI,GridIndexJ," ARE YOU SURE THIS SHOULD BE A GLACIER POINT?"
       !CALL wrf_debug(10,TRIM(message))
    endif

    end associate

  end subroutine WaterMainGlacier

end module WaterMainGlacierMod
