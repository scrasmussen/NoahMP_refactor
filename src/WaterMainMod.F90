module WaterMainMod

!!! Main water module including all water relevant processes
!!! canopy water -> snowpack water -> soil water -> ground water

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use CanopyHydrologyMod,  only : CanopyHydrology
  use SnowWaterMainMod,    only : SnowWaterMain
  use IrrigationFloodMod,  only : FloodIrrigation
  use IrrigationMicroMod,  only : MicroIrrigation
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
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake 
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              CROPLU          => noahmp%config%domain%CROPLU         ,& ! in,     flag to identify croplands
              QVAP            => noahmp%water%flux%QVAP              ,& ! in,     soil surface evaporation rate[mm/s]
              QDEW            => noahmp%water%flux%QDEW              ,& ! in,     soil surface dew rate[mm/s]
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
              BTRANI          => noahmp%water%state%BTRANI           ,& ! in,     soil water transpiration factor (0 to 1)
              WSLMAX          => noahmp%water%param%WSLMAX           ,& ! in,     maximum lake water storage (mm)
              NROOT           => noahmp%water%param%NROOT            ,& ! in,     number of soil layers with root present
              FROZEN_GROUND   => noahmp%energy%state%FROZEN_GROUND   ,& ! in,     frozen ground (logical) to define latent heat pathway
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              WSLAKE          => noahmp%water%state%WSLAKE           ,& ! inout,  water storage in lake (can be -) (mm)
              PONDING         => noahmp%water%state%PONDING          ,& ! inout,  melting water from snow when there is no layer (mm)
              sfcheadrt       => noahmp%water%state%sfcheadrt        ,& ! inout,  surface water head (mm) 
              IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
              IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
              IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! inout,  flood irrigation water rate [m/timestep]
              IRMIRATE        => noahmp%water%flux%IRMIRATE          ,& ! inout,  micro irrigation water rate [m/timestep]
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! inout,  water input on soil surface [mm/s]
              QSEVA           => noahmp%water%flux%QSEVA             ,& ! inout,  evaporation from soil surface [mm/s]
              QSDEW           => noahmp%water%flux%QSDEW             ,& ! inout,  soil surface dew rate [mm/s]
              QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! inout,  snow surface frost rate[mm/s]
              QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! inout,  snow surface sublimation rate[mm/s]
              ETRANI          => noahmp%water%flux%ETRANI            ,& ! inout,  evapotranspiration from soil layers [mm/s]
              SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! inout,  glacier flow [mm/s]
              ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,    transpiration rate (mm/s) [+]
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s]
              QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
              PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
              PONDING2        => noahmp%water%state%PONDING2          & ! out,    surface ponding 2 (mm)
             )
! ----------------------------------------------------------------------

    ! initialize
    ETRANI(:) = 0.0
    SNOFLOW   = 0.0
    RUNSUB    = 0.0
    QINSUR    = 0.0

    ! canopy-intercepted snowfall/rainfall, drips, and throughfall
    call CanopyHydrology(noahmp)

    ! ground sublimation and evaporation
    QSNSUB = 0.0
    if ( SNEQV > 0.0 ) then
       QSNSUB = min( QVAP, SNEQV/DT )
    endif
    QSEVA = QVAP - QSNSUB

    ! ground frost and dew
    QSNFRO = 0.0
    if ( SNEQV > 0.0 ) then
       QSNFRO = QDEW
    endif
    QSDEW = QDEW - QSNFRO

    ! snowpack water processs
    call SnowWaterMain(noahmp)

    ! treat frozen ground/soil
    if ( FROZEN_GROUND .eqv. .true. ) then
       SICE(1) =  SICE(1) + (QSDEW-QSEVA) * DT / (DZSNSO(1)*1000.0)
       QSDEW = 0.0
       QSEVA = 0.0
       if ( SICE(1) < 0.0 ) then
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.0
       endif
    endif
    QSEVA  = QSEVA * 0.001 ! mm/s -> m/s

    ! evapotranspiration
    do IZ = 1, NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    enddo

    ! total surface input water to soil
    QINSUR = (PONDING + PONDING1 + PONDING2) / DT * 0.001  ! convert units (mm/s -> m/s)
    if ( ISNOW == 0 ) then
       QINSUR = QINSUR + (QSNBOT + QSDEW + QRAIN) * 0.001
    else
       QINSUR = QINSUR + (QSNBOT + QSDEW) * 0.001
    endif
#ifdef WRF_HYDRO
    QINSUR = QINSUR + sfcheadrt / DT * 0.001
#endif

    ! irrigation: call flood irrigation and add to QINSUR
    if ( (CROPLU .eqv. .true.) .and. (IRAMTFI > 0.0) ) then
       call FloodIrrigation(noahmp)
       QINSUR = QINSUR + (IRFIRATE / DT)  ! [m/s]
    endif

    ! irrigation: call micro irrigation assuming we implement drip in first layer
    ! of the Noah-MP. Change layer 1 moisture wrt to MI rate
    if ( (CROPLU .eqv. .true.) .and. (IRAMTMI > 0.0) ) then
       call MicroIrrigation(noahmp)
       SH2O(1) = SH2O(1) + ( IRMIRATE / (-1.0*ZSOIL(1)) )
    endif

    ! lake/soil water balances
    if ( IST == 2 ) then   ! lake
       RUNSRF = 0.0
       if ( WSLAKE >= WSLMAX ) RUNSRF = QINSUR * 1000.0   ! mm/s
       WSLAKE = WSLAKE + (QINSUR-QSEVA) * 1000.0 * DT - RUNSRF * DT   !mm
    else                   ! soil
       ! soil water processes (including groundwater and shallow water MMF update)
       call SoilWaterMain(noahmp)
    endif

    ! merge excess glacier snow flow to subsurface runoff
    RUNSUB = RUNSUB + SNOFLOW         !mm/s

    end associate

  end subroutine WaterMain

end module WaterMainMod
