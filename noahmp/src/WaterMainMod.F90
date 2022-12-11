module WaterMainMod

!!! Main water module including all water relevant processes
!!! canopy water -> snowpack water -> soil water -> ground water

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use CanopyHydrologyMod,  only : CanopyHydrology
  use SnowWaterMainMod,    only : SnowWaterMain
  use IrrigationFloodMod,  only : IrrigationFlood
  use IrrigationMicroMod,  only : IrrigationMicro
  use SoilWaterMainMod,    only : SoilWaterMain

  implicit none

contains

  subroutine WaterMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: WATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July 2022)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd      ! loop index

! --------------------------------------------------------------------
    associate(                                                                       &
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,    noahmp main time step [s]
              SurfaceType            => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake 
              FlagCropland           => noahmp%config%domain%FlagCropland           ,& ! in,    flag to identify croplands
              FlagUrban              => noahmp%config%domain%FlagUrban              ,& ! in,    urban point flag
              VaporizeGrd            => noahmp%water%flux%VaporizeGrd               ,& ! in,    ground vaporize rate total (evap+sublim) [mm/s]
              CondenseVapGrd         => noahmp%water%flux%CondenseVapGrd            ,& ! in,    ground vapor condense rate total (dew+frost) [mm/s]
              RainfallGround         => noahmp%water%flux%RainfallGround            ,& ! in,    ground surface rain rate [mm/s]
              SoilTranspFac          => noahmp%water%state%SoilTranspFac            ,& ! in,    soil water transpiration factor (0 to 1)
              WaterStorageLakeMax    => noahmp%water%param%WaterStorageLakeMax      ,& ! in,    maximum lake water storage [mm]
              NumSoilLayerRoot       => noahmp%water%param%NumSoilLayerRoot         ,& ! in,    number of soil layers with root present
              FlagFrozenGround       => noahmp%energy%state%FlagFrozenGround        ,& ! in,    frozen ground (logical) to define latent heat pathway
              LatHeatVapGrd          => noahmp%energy%state%LatHeatVapGrd           ,& ! in,    latent heat of vaporization/subli [J/kg], ground
              DensityAirRefHeight    => noahmp%energy%state%DensityAirRefHeight     ,& ! in,    density air [kg/m3]
              ExchCoeffShSfc         => noahmp%energy%state%ExchCoeffShSfc          ,& ! in,    exchange coefficient [m/s] for heat, surface, grid mean
              SpecHumidityRefHeight  => noahmp%forcing%SpecHumidityRefHeight        ,& ! in,    specific humidity [kg/kg] at reference height
              HeatLatentGrd          => noahmp%energy%flux%HeatLatentGrd            ,& ! in,    total ground latent heat [W/m2] (+ to atm)
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              SnowWaterEquiv         => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              SnowWaterEquivPrev     => noahmp%water%state%SnowWaterEquivPrev       ,& ! inout, snow water equivalent at last time step [mm]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilIce                => noahmp%water%state%SoilIce                  ,& ! inout, soil ice moisture [m3/m3]
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! inout, total soil moisture [m3/m3]
              WaterStorageLake       => noahmp%water%state%WaterStorageLake         ,& ! inout, water storage in lake (can be negative) [mm]
              PondSfcThinSnwMelt     => noahmp%water%state%PondSfcThinSnwMelt       ,& ! inout, surface ponding [mm] from snowmelt when thin snow has no layer
              WaterHeadSfc           => noahmp%water%state%WaterHeadSfc             ,& ! inout, surface water head (mm) 
              IrrigationAmtFlood     => noahmp%water%state%IrrigationAmtFlood       ,& ! inout, flood irrigation water amount [m]
              IrrigationAmtMicro     => noahmp%water%state%IrrigationAmtMicro       ,& ! inout, micro irrigation water amount [m]
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow             ,& ! inout, water input on soil surface [mm/s]
              EvapSoilSfcLiq         => noahmp%water%flux%EvapSoilSfcLiq            ,& ! inout, evaporation from soil surface [mm/s]
              DewSoilSfcLiq          => noahmp%water%flux%DewSoilSfcLiq             ,& ! inout, soil surface dew rate [mm/s]
              FrostSnowSfcIce        => noahmp%water%flux%FrostSnowSfcIce           ,& ! inout, snow surface frost rate[mm/s]
              SublimSnowSfcIce       => noahmp%water%flux%SublimSnowSfcIce          ,& ! inout, snow surface sublimation rate[mm/s]
              TranspWatLossSoil      => noahmp%water%flux%TranspWatLossSoil         ,& ! inout, transpiration water loss from soil layers [mm/s]
              GlacierExcessFlow      => noahmp%water%flux%GlacierExcessFlow         ,& ! inout, glacier excess flow [mm/s]
              SpecHumidity2mBare     => noahmp%energy%state%SpecHumidity2mBare      ,& ! out,   bare ground 2-m specific humidity [kg/kg]
              SpecHumiditySfcBare    => noahmp%energy%state%SpecHumiditySfcBare     ,& ! out,   specific humidity at bare surface [kg/kg]
              EvapGroundNet          => noahmp%water%flux%EvapGroundNet             ,& ! out,   net ground (soil/snow) evaporation [mm/s]
              Transpiration          => noahmp%water%flux%Transpiration             ,& ! out,   transpiration rate [mm/s]
              EvapCanopyNet          => noahmp%water%flux%EvapCanopyNet             ,& ! out,   evaporation of intercepted water [mm/s]
              RunoffSurface          => noahmp%water%flux%RunoffSurface             ,& ! out,   surface runoff [mm/s]
              RunoffSubsurface       => noahmp%water%flux%RunoffSubsurface          ,& ! out,   subsurface runoff [mm/s]
              SnowBotOutflow         => noahmp%water%flux%SnowBotOutflow            ,& ! out,   total water (snowmelt+rain through pack) out of snow bottom [mm/s]
              WaterToAtmosTotal      => noahmp%water%flux%WaterToAtmosTotal         ,& ! out,   total water vapor flux to atmosphere [mm/s]
              PondSfcThinSnwComb     => noahmp%water%state%PondSfcThinSnwComb       ,& ! out,   surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans    => noahmp%water%state%PondSfcThinSnwTrans       & ! out,   surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

    ! initialize
    TranspWatLossSoil  = 0.0
    GlacierExcessFlow  = 0.0
    RunoffSubsurface   = 0.0
    SoilSfcInflow      = 0.0

    ! prepare for water process
    SoilIce(:)         = max(0.0, SoilMoisture(:)-SoilLiqWater(:))
    SnowWaterEquivPrev = SnowWaterEquiv
    ! compute soil/snow surface evap/dew rate based on energy flux
    VaporizeGrd        = max(HeatLatentGrd/LatHeatVapGrd, 0.0)       ! positive part of ground latent heat; Barlage change to ground v3.6
    CondenseVapGrd     = abs(min(HeatLatentGrd/LatHeatVapGrd, 0.0))  ! negative part of ground latent heat
    EvapGroundNet      = VaporizeGrd - CondenseVapGrd

    ! canopy-intercepted snowfall/rainfall, drips, and throughfall
    call CanopyHydrology(noahmp)

    ! ground sublimation and evaporation
    SublimSnowSfcIce    = 0.0
    if ( SnowWaterEquiv > 0.0 ) then
       SublimSnowSfcIce = min(VaporizeGrd, SnowWaterEquiv/MainTimeStep)
    endif
    EvapSoilSfcLiq      = VaporizeGrd - SublimSnowSfcIce

    ! ground frost and dew
    FrostSnowSfcIce     = 0.0
    if ( SnowWaterEquiv > 0.0 ) then
       FrostSnowSfcIce  = CondenseVapGrd
    endif
    DewSoilSfcLiq       = CondenseVapGrd - FrostSnowSfcIce

    ! snowpack water processs
    call SnowWaterMain(noahmp)

    ! treat frozen ground/soil
    if ( FlagFrozenGround .eqv. .true. ) then
       SoilIce(1)     = SoilIce(1) + (DewSoilSfcLiq-EvapSoilSfcLiq) * MainTimeStep / &
                                     (ThicknessSnowSoilLayer(1)*1000.0)
       DewSoilSfcLiq  = 0.0
       EvapSoilSfcLiq = 0.0
       if ( SoilIce(1) < 0.0 ) then
          SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
          SoilIce(1)      = 0.0
       endif
    endif
    EvapSoilSfcLiq = EvapSoilSfcLiq * 0.001 ! mm/s -> m/s

    ! transpiration
    do LoopInd = 1, NumSoilLayerRoot
       TranspWatLossSoil(LoopInd) = Transpiration * SoilTranspFac(LoopInd) * 0.001
    enddo

    ! total surface input water to soil
    SoilSfcInflow    = (PondSfcThinSnwMelt + PondSfcThinSnwComb + PondSfcThinSnwTrans) / &
                       MainTimeStep * 0.001  ! convert units (mm/s -> m/s)
    if ( NumSnowLayerNeg == 0 ) then
       SoilSfcInflow = SoilSfcInflow + (SnowBotOutflow + DewSoilSfcLiq + RainfallGround) * 0.001
    else
       SoilSfcInflow = SoilSfcInflow + (SnowBotOutflow + DewSoilSfcLiq) * 0.001
    endif

#ifdef WRF_HYDRO
    SoilSfcInflow    = SoilSfcInflow + WaterHeadSfc / MainTimeStep * 0.001
#endif

    ! irrigation: call flood irrigation and add to SoilSfcInflow
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtFlood > 0.0) ) call IrrigationFlood(noahmp)

    ! irrigation: call micro irrigation assuming we implement drip in first layer
    ! of the Noah-MP. Change layer 1 moisture wrt to MI rate
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtMicro > 0.0) ) call IrrigationMicro(noahmp)

    ! lake/soil water balances
    if ( SurfaceType == 2 ) then   ! lake
       RunoffSurface = 0.0
       if ( WaterStorageLake >= WaterStorageLakeMax ) RunoffSurface = SoilSfcInflow * 1000.0   ! mm/s
       WaterStorageLake = WaterStorageLake + (SoilSfcInflow-EvapSoilSfcLiq) * 1000.0 * MainTimeStep - &
                          RunoffSurface * MainTimeStep   !mm
    else                           ! soil
       ! soil water processes (including groundwater and shallow water MMF update)
       call SoilWaterMain(noahmp)
    endif

    ! merge excess glacier snow flow to subsurface runoff
    RunoffSubsurface = RunoffSubsurface + GlacierExcessFlow         !mm/s

    ! update surface water vapor flux ! urban - jref
    WaterToAtmosTotal = Transpiration + EvapCanopyNet + EvapGroundNet
    if ( (FlagUrban .eqv. .true.) ) then
       SpecHumiditySfcBare = WaterToAtmosTotal / (DensityAirRefHeight*ExchCoeffShSfc) + SpecHumidityRefHeight
       SpecHumidity2mBare  = SpecHumiditySfcBare
    endif

    end associate

  end subroutine WaterMain

end module WaterMainMod
