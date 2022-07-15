module SoilWaterMainMod

!!! Main soil water module including all soil water processes & update soil moisture
!!! surface runoff, infiltration, soil water diffusion, subsurface runoff, tile drainage

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use RunoffSurfaceTopModelGrdMod,       only : RunoffSurfaceTopModelGrd
  use RunoffSurfaceTopModelEquiMod,      only : RunoffSurfaceTopModelEqui
  use RunoffSurfaceFreeDrainMod,         only : RunoffSurfaceFreeDrain
  use RunoffSurfaceBatsMod,              only : RunoffSurfaceBATS
  use RunoffSurfaceTopModelMmfMod,       only : RunoffSurfaceTopModelMMF
  use RunoffSurfaceVicMod,               only : RunoffSurfaceVIC
  use RunoffSurfaceXinAnJiangMod,        only : RunoffSurfaceXinAnJiang
  use RunoffSurfaceDynamicVicMod,        only : RunoffSurfaceDynamicVic
  use RunoffSubSurfaceEquiWaterTableMod, only : RunoffSubSurfaceEquiWaterTable
  use RunoffSubSurfaceGroundWaterMod,    only : RunoffSubSurfaceGroundWater
  use RunoffSubSurfaceDrainageMod,       only : RunoffSubSurfaceDrainage
  use RunoffSubSurfaceShallowMmfMod,     only : RunoffSubSurfaceShallowWaterMMF
  use SoilWaterDiffusionRichardsMod,     only : SoilWaterDiffusionRichards
  use SoilMoistureSolverMod,             only : SoilMoistureSolver
  use TileDrainageSimpleMod,             only : TileDrainageSimple
  use TileDrainageHooghoudtMod,          only : TileDrainageHooghoudt

  implicit none

contains

  subroutine SoilWaterMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SOILWATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: K, IZ, ITER   ! do-loop index
    integer                :: NITER         ! iteration times soil moisture (-)
    real(kind=kind_noahmp) :: DTFINE        ! fine time step (s)
    real(kind=kind_noahmp) :: RSAT          ! accumulation of soil saturation excess [m]
    real(kind=kind_noahmp) :: WTSUB         ! sum of SoilWatConductivity(K)*ThicknessSnowSoilLayer(K)
    real(kind=kind_noahmp) :: MH2O          ! water mass removal (mm)
    real(kind=kind_noahmp) :: XS            ! temporary
    real(kind=kind_noahmp) :: WATMIN        ! minimum soil water
    real(kind=kind_noahmp) :: QDRAIN_SAVE   ! accumulated drainage water at fine time step
    real(kind=kind_noahmp) :: RUNSRF_SAVE   ! accumulated surface runoff at fine time step
    real(kind=kind_noahmp) :: FACC          ! accumulated infiltration rate (m/s)
    real(kind=kind_noahmp), parameter :: A = 4.0
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHSTT  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: AI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: BI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: CI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: MLIQ   ! water mass removal (mm)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,     thickness of snow/soil layers (m)
              FlagUrban      => noahmp%config%domain%FlagUrban     ,& ! in,     logical flag for urban grid
              OptRunoffSurface => noahmp%config%nmlist%OptRunoffSurface ,& ! in,     options for surface runoff
              OptRunoffSubsurface => noahmp%config%nmlist%OptRunoffSubsurface,& ! in,     options for subsurface runoff
              OptTileDrainage => noahmp%config%nmlist%OptTileDrainage,& ! in,     options for tile drainage
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,     soil ice content [m3/m3]
              TileDrainFrac        => noahmp%water%state%TileDrainFrac         ,& ! in,     tile drainage map(fraction)
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil water content [m3/m3]
              SoilMoisture             => noahmp%water%state%SoilMoisture          ,& ! inout,  total soil water content [m3/m3]
              RechargeGwDeepWT        => noahmp%water%state%RechargeGwDeepWT         ,& ! inout,  recharge to or from the water table when deep [m]
              QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! out,    soil bottom drainage (m/s)
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s] 
              PDDUM           => noahmp%water%flux%PDDUM             ,& ! out,    infiltration rate at surface (mm/s)
              SoilImpervFracMax          => noahmp%water%state%SoilImpervFracMax           ,& ! out,    maximum soil imperviousness fraction
              SoilWatConductivity            => noahmp%water%state%SoilWatConductivity             ,& ! out,    soil hydraulic conductivity [m/s]
              SoilEffPorosity           => noahmp%water%state%SoilEffPorosity       ,& ! out,    soil effective porosity (m3/m3)
              SoilImpervFrac             => noahmp%water%state%SoilImpervFrac              ,& ! out,    impervious fraction due to frozen soil
              SoilIceFrac            => noahmp%water%state%SoilIceFrac        ,& ! out,    ice fraction in frozen soil
              SoilSaturationExcess           => noahmp%water%state%SoilSaturationExcess            ,& ! out,    saturation excess of the total soil [m]
              SoilIceMax         => noahmp%water%state%SoilIceMax          ,& ! out,    maximum soil ice content (m3/m3)
              SoilLiqWaterMin         => noahmp%water%state%SoilLiqWaterMin           & ! out,    minimum soil liquid water content (m3/m3)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTT (1:NumSoilLayer) )
    allocate( AI    (1:NumSoilLayer) )
    allocate( BI    (1:NumSoilLayer) )
    allocate( CI    (1:NumSoilLayer) )
    allocate( MLIQ  (1:NumSoilLayer) )
    RHSTT  = 0.0
    AI     = 0.0
    BI     = 0.0
    CI     = 0.0
    MLIQ   = 0.0
    RUNSRF = 0.0
    RUNSUB = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0
    FACC   = 1.0e-06

    ! for the case when snowmelt water is too large
    do K = 1, NumSoilLayer
       SoilEffPorosity(K)= max( 1.0e-4, (SMCMAX(K) - SoilIce(K)) )
       RSAT    = RSAT + max( 0.0, SoilLiqWater(K) - SoilEffPorosity(K) ) * ThicknessSnowSoilLayer(K)
       SoilLiqWater(K) = min( SoilEffPorosity(K), SoilLiqWater(K) )
    enddo

    ! impermeable fraction due to frozen soil
    do K = 1, NumSoilLayer
       SoilIceFrac(K) = min( 1.0, SoilIce(K) / SMCMAX(K) )
       SoilImpervFrac(K)  = max( 0.0, exp(-A*(1.0-SoilIceFrac(K))) - exp(-A) ) / (1.0 - exp(-A))
    enddo

    ! maximum soil ice content and minimum liquid water of all layers
    SoilIceMax = 0.0
    SoilImpervFracMax  = 0.0
    SoilLiqWaterMin = SMCMAX(1)
    do K = 1, NumSoilLayer
       if ( SoilIce(K) > SoilIceMax ) SoilIceMax = SoilIce(K)
       if ( SoilImpervFrac(K)  > SoilImpervFracMax  ) SoilImpervFracMax  = SoilImpervFrac(K)
       if ( SoilLiqWater(K) < SoilLiqWaterMin ) SoilLiqWaterMin = SoilLiqWater(K)
    enddo

    ! subsurface runoff for runoff scheme option 2
    if ( OptRunoffSubsurface == 2 ) call RunoffSubSurfaceEquiWaterTable(noahmp)

    !!! surface runoff and infiltration rate using different schemes
    ! jref impermable surface at urban
    if ( FlagUrban .eqv. .true. ) SoilImpervFrac(1) = 0.95

    if ( OptRunoffSurface == 1 ) call RunoffSurfaceTopModelGrd(noahmp)
    if ( OptRunoffSurface == 2 ) call RunoffSurfaceTopModelEqui(noahmp)
    if ( OptRunoffSurface == 3 ) call RunoffSurfaceFreeDrain(noahmp,MainTimeStep)
    if ( OptRunoffSurface == 4 ) call RunoffSurfaceBATS(noahmp)
    if ( OptRunoffSurface == 5 ) call RunoffSurfaceTopModelMMF(noahmp)
    if ( OptRunoffSurface == 6 ) call RunoffSurfaceVIC(noahmp,MainTimeStep)
    if ( OptRunoffSurface == 7 ) call RunoffSurfaceXinAnJiang(noahmp,MainTimeStep)
    if ( OptRunoffSurface == 8 ) call RunoffSurfaceDynamicVic(noahmp,MainTimeStep,FACC)

    ! determine iteration times  to solve soil water diffusion and moisture
    NITER = 3
    if ( (PDDUM*MainTimeStep) > (ThicknessSnowSoilLayer(1)*SMCMAX(1)) ) then
       NITER = NITER*2
    endif
    DTFINE  = MainTimeStep / NITER

    ! solve soil moisture
    FACC        = 1.0e-06
    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0

    do ITER = 1, NITER
       if ( QINSUR > 0.0 ) then
          if ( OptRunoffSurface == 3 ) call RunoffSurfaceFreeDrain(noahmp,DTFINE)
          if ( OptRunoffSurface == 6 ) call RunoffSurfaceVIC(noahmp,DTFINE)
          if ( OptRunoffSurface == 7 ) call RunoffSurfaceXinAnJiang(noahmp,DTFINE)
          if ( OptRunoffSurface == 8 ) call RunoffSurfaceDynamicVic(noahmp,DTFINE,FACC)
       endif
       call SoilWaterDiffusionRichards(noahmp, AI, BI, CI, RHSTT)
       call SoilMoistureSolver(noahmp, DTFINE, AI, BI, CI, RHSTT)
       RSAT        = RSAT + SoilSaturationExcess
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + RUNSRF
    enddo

    QDRAIN = QDRAIN_SAVE / NITER
    RUNSRF = RUNSRF_SAVE / NITER
    RUNSRF = RUNSRF * 1000.0 + RSAT * 1000.0 / MainTimeStep  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.0

    ! compute tile drainage ! pvk
    if ( (OptTileDrainage == 1) .and. (TileDrainFrac > 0.3) .and. (OptRunoffSurface == 3) ) then
       call TileDrainageSimple(noahmp)  ! simple tile drainage
    endif
    if ( (OptTileDrainage == 2) .and. (TileDrainFrac > 0.1) .and. (OptRunoffSurface == 3) ) then
       call TileDrainageHooghoudt(noahmp)  ! Hooghoudt tile drain
    END IF

    ! removal of soil water due to subsurface runoff (option 2)
    if ( OptRunoffSubsurface == 2 ) then
       WTSUB = 0.0
       do K = 1, NumSoilLayer
          WTSUB = WTSUB + SoilWatConductivity(K) * ThicknessSnowSoilLayer(K)
       enddo
       do K = 1, NumSoilLayer
          MH2O    = RUNSUB * MainTimeStep * (SoilWatConductivity(K)*ThicknessSnowSoilLayer(K)) / WTSUB  ! mm
          SoilLiqWater(K) = SoilLiqWater(K) - MH2O / (ThicknessSnowSoilLayer(K)*1000.0)
       enddo
    endif

    ! Limit MLIQ to be greater than or equal to watmin.
    ! Get water needed to bring MLIQ equal WATMIN from lower layer.
    if ( OptRunoffSubsurface /= 1 ) then
       do IZ = 1, NumSoilLayer
          MLIQ(IZ) = SoilLiqWater(IZ) * ThicknessSnowSoilLayer(IZ) * 1000.0
       enddo

       WATMIN = 0.01   ! mm
       do IZ = 1, NumSoilLayer-1
          if ( MLIQ(IZ) < 0.0 ) then
             XS = WATMIN - MLIQ(IZ)
          else
             XS = 0.0
          endif
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
       enddo
       IZ = NumSoilLayer
       if ( MLIQ(IZ) < WATMIN ) then
           XS = WATMIN - MLIQ(IZ)
       else
           XS = 0.0
       endif
       MLIQ(IZ) = MLIQ(IZ) + XS
       RUNSUB   = RUNSUB - XS/MainTimeStep

       if ( OptRunoffSubsurface == 5 ) RechargeGwDeepWT = RechargeGwDeepWT - XS * 1.0e-3

       do IZ = 1, NumSoilLayer
          SoilLiqWater(IZ) = MLIQ(IZ) / (ThicknessSnowSoilLayer(IZ)*1000.0)
       enddo
    endif ! OptRunoffSubsurface /= 1

    ! compute groundwater and subsurface runoff
    if ( OptRunoffSubsurface == 1 ) call RunoffSubSurfaceGroundWater(noahmp)

    ! compute subsurface runoff based on drainage rate
    if ( (OptRunoffSubsurface == 3) .or. (OptRunoffSubsurface == 4) .or. (OptRunoffSubsurface == 6) .or. &
         (OptRunoffSubsurface == 7) .or. (OptRunoffSubsurface == 8) ) then
         call RunoffSubSurfaceDrainage(noahmp)
    endif

    ! update soil moisture
    do IZ = 1, NumSoilLayer
        SoilMoisture(IZ) = SoilLiqWater(IZ) + SoilIce(IZ)
    enddo

    ! compute subsurface runoff and shallow water table for MMF scheme
    if ( OptRunoffSubsurface == 5 ) call RunoffSubSurfaceShallowWaterMMF(noahmp)

    end associate

  end subroutine SoilWaterMain

end module SoilWaterMainMod
