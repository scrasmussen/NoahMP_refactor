module SnowWaterMainGlacierMod

!!! Main glacier snow water module including all snowpack processes
!!! Snowfall -> Snowpack compaction -> Snow layer combination -> Snow layer division -> Snow Hydrology

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowfallGlacierMod,            only : SnowfallGlacier
  use SnowpackCompactionGlacierMod,  only : SnowpackCompactionGlacier
  use SnowLayerCombineGlacierMod,    only : SnowLayerCombineGlacier
  use SnowLayerDivideGlacierMod,     only : SnowLayerDivideGlacier
  use SnowpackHydrologyGlacierMod,   only : SnowpackHydrologyGlacier

  implicit none

contains

  subroutine SnowWaterMainGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWWATER_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ        ! do loop/array indices
    real(kind=kind_noahmp) :: BDSNOW    ! bulk density of snow (kg/m3)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,& ! in,    maximum number of snow layers
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,& ! in,    number of soil layers
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    noahmp main time step (s)
              DepthSoilLayer  => noahmp%config%domain%DepthSoilLayer ,& ! in,    depth [m] of layer-bottom from soil surface
              SWEMAXGLA       => noahmp%water%param%SWEMAXGLA        ,& ! in,    Maximum SWE allowed at glaciers (mm)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! inout, thickness of snow/soil layers (m)
              DepthSnowSoilLayer           => noahmp%config%domain%DepthSnowSoilLayer          ,& ! inout, depth of snow/soil layer-bottom (m)
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg ,& ! inout, actual number of snow layers (negative)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout, snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout, snow water equivalent [mm]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout, snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout, snow layer liquid water [mm]
              STC             => noahmp%energy%state%STC             ,& ! inout, snow and soil layer temperature [k]
              SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! out,   glacier flow [mm/s]
              PONDING1        => noahmp%water%state%PONDING1         ,& ! out,   surface ponding 1 (mm)
              PONDING2        => noahmp%water%state%PONDING2          & ! out,   surface ponding 2 (mm)
             )
! ----------------------------------------------------------------------

    ! initialize out-only variables
    SNOFLOW  = 0.0
    PONDING1 = 0.0
    PONDING2 = 0.0

    ! snowfall
    call SnowfallGlacier(noahmp)

    ! do following snow layer compaction, combination, and division only for multi-layer snowpack

    ! snowpack compaction
    if ( NumSnowLayerNeg < 0 ) call SnowpackCompactionGlacier(noahmp)

    ! snow layer combination
    if ( NumSnowLayerNeg < 0 ) call SnowLayerCombineGlacier(noahmp)

    ! snow layer division
    if ( NumSnowLayerNeg < 0 ) call SnowLayerDivideGlacier(noahmp)

    ! snow hydrology for all snow cases
    call SnowpackHydrologyGlacier(noahmp)

    ! set empty snow layer properties to zero
    do IZ = -NumSnowLayerMax+1, NumSnowLayerNeg
       SNICE(IZ)  = 0.0
       SNLIQ(IZ)  = 0.0
       STC(IZ)    = 0.0
       ThicknessSnowSoilLayer(IZ) = 0.0
       DepthSnowSoilLayer(IZ)  = 0.0
    enddo

    ! to obtain equilibrium state of snow in glacier region
    if ( SNEQV > SWEMAXGLA ) then  ! SWEMAXGLA: 5000 mm -> maximum SWE
       BDSNOW      = SNICE(0) / ThicknessSnowSoilLayer(0)
       SNOFLOW     = SNEQV - SWEMAXGLA
       SNICE(0)    = SNICE(0)  - SNOFLOW
       ThicknessSnowSoilLayer(0)   = ThicknessSnowSoilLayer(0) - SNOFLOW / BDSNOW
       SNOFLOW     = SNOFLOW / MainTimeStep
    endif

    ! sum up snow mass for layered snow
    if ( NumSnowLayerNeg < 0 ) then  ! MB: only do for multi-layer
       SNEQV = 0.0
       do IZ = NumSnowLayerNeg+1, 0
          SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       enddo
    endif

    ! Reset DepthSnowSoilLayer and layer thinkness ThicknessSnowSoilLayer
    do IZ = NumSnowLayerNeg+1, 0
       ThicknessSnowSoilLayer(IZ) = -ThicknessSnowSoilLayer(IZ)
    enddo
    ThicknessSnowSoilLayer(1) = DepthSoilLayer(1)
    do IZ = 2, NumSoilLayer
       ThicknessSnowSoilLayer(IZ) = ( DepthSoilLayer(IZ) - DepthSoilLayer(IZ-1) )
    enddo
    DepthSnowSoilLayer(NumSnowLayerNeg+1) = ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    do IZ = NumSnowLayerNeg+2, NumSoilLayer
       DepthSnowSoilLayer(IZ) = DepthSnowSoilLayer(IZ-1) + ThicknessSnowSoilLayer(IZ)
    enddo
    do IZ = NumSnowLayerNeg+1, NumSoilLayer
       ThicknessSnowSoilLayer(IZ) = -ThicknessSnowSoilLayer(IZ)
    enddo

    ! update snow quantity
    if ( (SNOWH <= 1.0e-6) .or. (SNEQV <= 1.0e-3) ) then
       SNOWH = 0.0
       SNEQV = 0.0
    endif

    end associate

  end subroutine SnowWaterMainGlacier

end module SnowWaterMainGlacierMod
