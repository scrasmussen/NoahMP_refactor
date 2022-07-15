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
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ      ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              SurfaceType           => noahmp%config%domain%SurfaceType   ,& ! in,     surface type 1-soil; 2-lake 
              FlagCropland          => noahmp%config%domain%FlagCropland         ,& ! in,     flag to identify croplands
              FlagUrban             => noahmp%config%domain%FlagUrban     ,& ! in,     urban point flag
              QVAP            => noahmp%water%flux%QVAP              ,& ! in,     soil surface evaporation rate[mm/s]
              QDEW            => noahmp%water%flux%QDEW              ,& ! in,     soil surface dew rate[mm/s]
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
              SoilTranspFac          => noahmp%water%state%SoilTranspFac           ,& ! in,     soil water transpiration factor (0 to 1)
              WSLMAX          => noahmp%water%param%WSLMAX           ,& ! in,     maximum lake water storage (mm)
              NROOT           => noahmp%water%param%NROOT            ,& ! in,     number of soil layers with root present
              FROZEN_GROUND   => noahmp%energy%state%FROZEN_GROUND   ,& ! in,     frozen ground (logical) to define latent heat pathway
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! in,     latent heat of vaporization/subli (j/kg), ground
              RHOAIR          => noahmp%energy%state%RHOAIR          ,& ! in,     density air (kg/m3)
              CH              => noahmp%energy%state%CH              ,& ! in,     exchange coefficient (m/s) for heat, surface, grid mean
              SpecHumidityRefHeight => noahmp%forcing%SpecHumidityRefHeight,& ! in,     specific humidity (kg/kg) at reference height
              FGEV            => noahmp%energy%flux%FGEV             ,& ! in,     soil evap heat (w/m2) [+ to atm]
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! inout,  actual number of snow layers (negative)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! inout,  thickness of snow/soil layers (m)
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout,  snow water equivalent [mm]
              SnowWaterEquivPrev          => noahmp%water%state%SnowWaterEquivPrev           ,& ! inout,  snow water equivalent at last time step (mm)
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil water content [m3/m3]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! inout,  soil ice moisture (m3/m3)
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! inout,  total soil moisture [m3/m3]
              WaterStorageLake          => noahmp%water%state%WaterStorageLake           ,& ! inout,  water storage in lake (can be negative) [mm]
              PondSfcThinSnwMelt         => noahmp%water%state%PondSfcThinSnwMelt          ,& ! inout,  surface ponding [mm] from snowmelt when thin snow has no layer
              WaterHeadSfc       => noahmp%water%state%WaterHeadSfc        ,& ! inout,  surface water head (mm) 
              IrrigationAmtFlood         => noahmp%water%state%IrrigationAmtFlood          ,& ! inout,  flood irrigation water amount [m]
              IrrigationAmtMicro         => noahmp%water%state%IrrigationAmtMicro          ,& ! inout,  micro irrigation water amount [m]
              IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! inout,  flood irrigation water rate [m/timestep]
              IRMIRATE        => noahmp%water%flux%IRMIRATE          ,& ! inout,  micro irrigation water rate [m/timestep]
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! inout,  water input on soil surface [mm/s]
              QSEVA           => noahmp%water%flux%QSEVA             ,& ! inout,  evaporation from soil surface [mm/s]
              QSDEW           => noahmp%water%flux%QSDEW             ,& ! inout,  soil surface dew rate [mm/s]
              QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! inout,  snow surface frost rate[mm/s]
              QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! inout,  snow surface sublimation rate[mm/s]
              ETRANI          => noahmp%water%flux%ETRANI            ,& ! inout,  evapotranspiration from soil layers [mm/s]
              SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! inout,  glacier flow [mm/s]
              Q2B             => noahmp%energy%state%Q2B             ,& ! out,    bare ground 2-m water vapor mixing ratio
              QSFC            => noahmp%energy%state%QSFC            ,& ! out,    water vapor mixing ratio bare ground
              EDIR            => noahmp%water%flux%EDIR              ,& ! out,    net direct soil evaporation (mm/s)
              ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,    transpiration rate (mm/s) [+]
              ECAN            => noahmp%water%flux%ECAN              ,& ! out,    evaporation of intercepted water (mm/s) [+]
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s]
              QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
              QFX             => noahmp%water%flux%QFX               ,& ! out,    total water vapor flux to atmosphere (mm/s)
              PondSfcThinSnwComb        => noahmp%water%state%PondSfcThinSnwComb         ,& ! out,    surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans        => noahmp%water%state%PondSfcThinSnwTrans          & ! out,   surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

    ! initialize
    ETRANI    = 0.0
    SNOFLOW   = 0.0
    RUNSUB    = 0.0
    QINSUR    = 0.0

    ! prepare for water process
    SoilIce(:) = max(0.0, SoilMoisture(:)-SoilLiqWater(:))
    SnowWaterEquivPrev  = SnowWaterEquiv
    ! compute soil/snow surface evap/dew rate based on energy flux
    QVAP    = max(FGEV/LATHEAG, 0.0)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW    = abs(min(FGEV/LATHEAG, 0.0))  ! negative part of fgev
    EDIR    = QVAP - QDEW

    ! canopy-intercepted snowfall/rainfall, drips, and throughfall
    call CanopyHydrology(noahmp)

    ! ground sublimation and evaporation
    QSNSUB = 0.0
    if ( SnowWaterEquiv > 0.0 ) then
       QSNSUB = min( QVAP, SnowWaterEquiv/MainTimeStep )
    endif
    QSEVA = QVAP - QSNSUB

    ! ground frost and dew
    QSNFRO = 0.0
    if ( SnowWaterEquiv > 0.0 ) then
       QSNFRO = QDEW
    endif
    QSDEW = QDEW - QSNFRO

    ! snowpack water processs
    call SnowWaterMain(noahmp)

    ! treat frozen ground/soil
    if ( FROZEN_GROUND .eqv. .true. ) then
       SoilIce(1) =  SoilIce(1) + (QSDEW-QSEVA) * MainTimeStep / (ThicknessSnowSoilLayer(1)*1000.0)
       QSDEW = 0.0
       QSEVA = 0.0
       if ( SoilIce(1) < 0.0 ) then
          SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
          SoilIce(1) = 0.0
       endif
    endif
    QSEVA  = QSEVA * 0.001 ! mm/s -> m/s

    ! evapotranspiration
    do IZ = 1, NROOT
       ETRANI(IZ) = ETRAN * SoilTranspFac(IZ) * 0.001
    enddo

    ! total surface input water to soil
    QINSUR = (PondSfcThinSnwMelt + PondSfcThinSnwComb + PondSfcThinSnwTrans) / MainTimeStep * 0.001  ! convert units (mm/s -> m/s)
    if ( NumSnowLayerNeg == 0 ) then
       QINSUR = QINSUR + (QSNBOT + QSDEW + QRAIN) * 0.001
    else
       QINSUR = QINSUR + (QSNBOT + QSDEW) * 0.001
    endif

#ifdef WRF_HYDRO
    QINSUR = QINSUR + WaterHeadSfc / MainTimeStep * 0.001
#endif

    ! irrigation: call flood irrigation and add to QINSUR
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtFlood > 0.0) ) call IrrigationFlood(noahmp)

    ! irrigation: call micro irrigation assuming we implement drip in first layer
    ! of the Noah-MP. Change layer 1 moisture wrt to MI rate
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtMicro > 0.0) ) call IrrigationMicro(noahmp)

    ! lake/soil water balances
    if ( SurfaceType == 2 ) then   ! lake
       RUNSRF = 0.0
       if ( WaterStorageLake >= WSLMAX ) RUNSRF = QINSUR * 1000.0   ! mm/s
       WaterStorageLake = WaterStorageLake + (QINSUR-QSEVA) * 1000.0 * MainTimeStep - RUNSRF * MainTimeStep   !mm
    else                   ! soil
       ! soil water processes (including groundwater and shallow water MMF update)
       call SoilWaterMain(noahmp)
    endif

    ! merge excess glacier snow flow to subsurface runoff
    RUNSUB = RUNSUB + SNOFLOW         !mm/s

    ! update surface water vapor flux ! urban - jref
    QFX = ETRAN + ECAN + EDIR
    if ( (FlagUrban .eqv. .true.) ) then
       QSFC = QFX / (RHOAIR * CH) + SpecHumidityRefHeight
       Q2B  = QSFC
    endif

    end associate

  end subroutine WaterMain

end module WaterMainMod
