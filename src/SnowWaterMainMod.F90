module SnowWaterMainMod

!!! Main snow water module including all snowpack processes
!!! Snowfall -> Snowpack compaction -> Snow layer combination -> Snow layer division -> Snow Hydrology

  use Machine, only : kind_noahmp
  use NoahmpType
  use ConstantDefineMod
  use SnowfallBelowCanopyMod, only : SnowfallAfterCanopyIntercept
  use SnowpackCompactionMod,  only : SnowpackCompaction
  use SnowLayerCombineMod,    only : SnowLayerCombine
  use SnowLayerDivideMod,     only : SnowLayerDivide
  use SnowpackHydrologyMod,   only : SnowpackHydrology

  implicit none

contains

  subroutine SnowWaterMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWWATER
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
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,     maximum number of snow layers
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! inout,  depth of snow/soil layer-bottom (m)
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! out,    glacier flow [mm/s]
              PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
              PONDING2        => noahmp%water%state%PONDING2          & ! out,    surface ponding 2 (mm)
             )
! ----------------------------------------------------------------------

! initialize out-only variables
    SNOFLOW  = 0.0
    PONDING1 = 0.0
    PONDING2 = 0.0

! snowfall after canopy interception
    call SnowfallAfterCanopyIntercept(noahmp)

! do following snow layer compaction, combination, and division only for multi-layer snowpack

! snowpack compaction
    if ( ISNOW < 0 ) call SnowpackCompaction(noahmp)

! snow layer combination
    if ( ISNOW < 0 ) call SnowLayerCombine(noahmp)

! snow layer division
    if ( ISNOW < 0 ) call SnowLayerDivide(noahmp)

! snow hydrology for all snow cases
    call SnowpackHydrology(noahmp)

! set empty snow layer properties to zero
    do IZ = -NSNOW+1, ISNOW
       SNICE(IZ)  = 0.0
       SNLIQ(IZ)  = 0.0
       STC(IZ)    = 0.0
       DZSNSO(IZ) = 0.0
       ZSNSO(IZ)  = 0.0
    enddo

! to obtain equilibrium state of snow in glacier region
    if ( SNEQV > 5000.0 ) then  ! 5000 mm -> maximum water depth
       BDSNOW      = SNICE(0) / DZSNSO(0)
       SNOFLOW     = SNEQV - 5000.0
       SNICE(0)    = SNICE(0)  - SNOFLOW
       DZSNSO(0)   = DZSNSO(0) - SNOFLOW / BDSNOW
       SNOFLOW     = SNOFLOW / DT
    endif

! sum up snow mass for layered snow
    if ( ISNOW < 0 ) then  ! MB: only do for multi-layer
       SNEQV = 0.0
       do IZ = ISNOW+1, 0
          SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       enddo
    endif

! Reset ZSNSO and layer thinkness DZSNSO
    do IZ = ISNOW+1, 0
       DZSNSO(IZ) = -DZSNSO(IZ)
    enddo
    DZSNSO(1) = ZSOIL(1)
    do IZ = 2, NSOIL
       DZSNSO(IZ) = ( ZSOIL(IZ) - ZSOIL(IZ-1) )
    enddo
    ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
    do IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
    enddo
    do IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
    enddo

    end associate

  end subroutine SnowWaterMain

end module SnowWaterMainMod
