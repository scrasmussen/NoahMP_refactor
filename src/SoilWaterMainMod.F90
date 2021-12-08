module SoilWaterMainMod

!!! Main soil water module including all soil water processes & update soil moisture
!!! surface runoff, infiltration, soil water diffusion, subsurface runoff, tile drainage

  use Machine, only : kind_noahmp
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
    real(kind=kind_noahmp) :: RSAT          ! accumulation of WPLUS (saturation excess) [m]
    real(kind=kind_noahmp) :: WTSUB         ! sum of WCND(K)*DZSNSO(K)
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
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              URBAN_FLAG      => noahmp%config%domain%URBAN_FLAG     ,& ! in,     logical flag for urban grid
              OPT_RUNSRF      => noahmp%config%nmlist%OPT_RUNSRF     ,& ! in,     options for surface runoff
              OPT_RUNSUB      => noahmp%config%nmlist%OPT_RUNSUB     ,& ! in,     options for subsurface runoff
              OPT_TDRN        => noahmp%config%nmlist%OPT_TDRN       ,& ! in,     options for tile drainage
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              TDFRACMP        => noahmp%water%state%TDFRACMP         ,& ! in,     tile drainage map(fraction)
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil water content [m3/m3]
              ZWT             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
              DEEPRECH        => noahmp%water%state%DEEPRECH         ,& ! inout,  recharge to or from the water table when deep [m]
              QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! out,    soil bottom drainage (m/s)
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s] 
              PDDUM           => noahmp%water%flux%PDDUM             ,& ! out,    infiltration rate at surface (mm/s)
              FCRMAX          => noahmp%water%state%FCRMAX           ,& ! out,    maximum fraction of imperviousness (FCR)
              WCND            => noahmp%water%state%WCND             ,& ! out,    soil hydraulic conductivity (m/s)
              EPORE           => noahmp%water%state%EPORE_SOIL       ,& ! out,    soil effective porosity (m3/m3)
              FCR             => noahmp%water%state%FCR              ,& ! out,    impermeable fraction due to frozen soil
              FICE            => noahmp%water%state%FICE_SOIL        ,& ! out,    ice fraction in frozen soil
              WPLUS           => noahmp%water%state%WPLUS            ,& ! out,    saturation excess of the total soil [m]
              SICEMAX         => noahmp%water%state%SICEMAX          ,& ! out,    maximum soil ice content (m3/m3)
              SH2OMIN         => noahmp%water%state%SH2OMIN           & ! out,    minimum soil liquid water content (m3/m3)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTT (1:NSOIL) )
    allocate( AI    (1:NSOIL) )
    allocate( BI    (1:NSOIL) )
    allocate( CI    (1:NSOIL) )
    allocate( MLIQ  (1:NSOIL) )
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
    do K = 1, NSOIL
       EPORE(K)= max( 1.0e-4, (SMCMAX(K) - SICE(K)) )
       RSAT    = RSAT + max( 0.0, SH2O(K) - EPORE(K) ) * DZSNSO(K)
       SH2O(K) = min( EPORE(K), SH2O(K) )
    enddo

    ! impermeable fraction due to frozen soil
    do K = 1, NSOIL
       FICE(K) = min( 1.0, SICE(K) / SMCMAX(K) )
       FCR(K)  = max( 0.0, exp(-A*(1.0-FICE(K))) - exp(-A) ) / (1.0 - exp(-A))
    enddo

    ! maximum soil ice content and minimum liquid water of all layers
    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = SMCMAX(1)
    do K = 1, NSOIL
       if ( SICE(K) > SICEMAX ) SICEMAX = SICE(K)
       if ( FCR(K)  > FCRMAX  ) FCRMAX  = FCR(K)
       if ( SH2O(K) < SH2OMIN ) SH2OMIN = SH2O(K)
    enddo

    ! subsurface runoff for runoff scheme option 2
    if ( OPT_RUNSUB == 2 ) call RunoffSubSurfaceEquiWaterTable(noahmp)

    !!! surface runoff and infiltration rate using different schemes
    ! jref impermable surface at urban
    if ( URBAN_FLAG .eqv. .true. ) FCR(1) = 0.95

    if ( OPT_RUNSRF == 1 ) call RunoffSurfaceTopModelGrd(noahmp)
    if ( OPT_RUNSRF == 2 ) call RunoffSurfaceTopModelEqui(noahmp)
    if ( OPT_RUNSRF == 3 ) call RunoffSurfaceFreeDrain(noahmp,DT)
    if ( OPT_RUNSRF == 4 ) call RunoffSurfaceBATS(noahmp)
    if ( OPT_RUNSRF == 5 ) call RunoffSurfaceTopModelMMF(noahmp)
    if ( OPT_RUNSRF == 6 ) call RunoffSurfaceVIC(noahmp,DT)
    if ( OPT_RUNSRF == 7 ) call RunoffSurfaceXinAnJiang(noahmp,DT)
    if ( OPT_RUNSRF == 8 ) call RunoffSurfaceDynamicVic(noahmp,DT,FACC)

    ! determine iteration times  to solve soil water diffusion and moisture
    NITER = 3
    if ( PDDUM*DT > DZSNSO(1)*SMCMAX(1) ) then
       NITER = NITER*2
    endif
    DTFINE  = DT / NITER

    ! solve soil moisture
    FACC        = 1.0e-06
    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0

    do ITER = 1, NITER
       if ( QINSUR > 0.0 ) then
          if ( OPT_RUNSRF == 3 ) call RunoffSurfaceFreeDrain(noahmp,DTFINE)
          if ( OPT_RUNSRF == 6 ) call RunoffSurfaceVIC(noahmp,DTFINE)
          if ( OPT_RUNSRF == 7 ) call RunoffSurfaceXinAnJiang(noahmp,DTFINE)
          if ( OPT_RUNSRF == 8 ) call RunoffSurfaceDynamicVic(noahmp,DTFINE,FACC)
       endif
       call SoilWaterDiffusionRichards(noahmp, DTFINE, AI, BI, CI, RHSTT)
       call SoilMoistureSolver(noahmp, DTFINE, AI, BI, CI, RHSTT)
       RSAT        = RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + RUNSRF
    enddo

    QDRAIN = QDRAIN_SAVE / NITER
    RUNSRF = RUNSRF_SAVE / NITER
    RUNSRF = RUNSRF * 1000.0 + RSAT * 1000.0 / DT  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.0

    ! compute tile drainage ! pvk
    if ( OPT_TDRN == 1 .and. TDFRACMP > 0.3 .and. OPT_RUNSRF == 3 ) then
       call TileDrainageSimple(noahmp)  ! simple tile drainage
    endif
    if ( OPT_TDRN == 2 .and. TDFRACMP > 0.1 .and. OPT_RUNSRF == 3 ) then
       call TileDrainageHooghoudt(noahmp)  ! Hooghoudt tile drain
    END IF

    ! removal of soil water due to subsurface runoff (option 2)
    if ( OPT_RUNSUB == 2 ) then
       WTSUB = 0.0
       do K = 1, NSOIL
          WTSUB = WTSUB + WCND(K) * DZSNSO(K)
       enddo
       do K = 1, NSOIL
          MH2O    = RUNSUB * DT * (WCND(K)*DZSNSO(K)) / WTSUB  ! mm
          SH2O(K) = SH2O(K) - MH2O / (DZSNSO(K)*1000.0)
       enddo
    endif

    ! Limit MLIQ to be greater than or equal to watmin.
    ! Get water needed to bring MLIQ equal WATMIN from lower layer.
    if ( OPT_RUNSUB /= 1 ) then
       do IZ = 1, NSOIL
          MLIQ(IZ) = SH2O(IZ) * DZSNSO(IZ) * 1000.0
       enddo

       WATMIN = 0.01   ! mm
       do IZ = 1, NSOIL-1
          if ( MLIQ(IZ) < 0.0 ) then
             XS = WATMIN - MLIQ(IZ)
          else
             XS = 0.0
          endif
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
       enddo
       IZ = NSOIL
       if ( MLIQ(IZ) < WATMIN ) then
           XS = WATMIN - MLIQ(IZ)
       else
           XS = 0.0
       endif
       MLIQ(IZ) = MLIQ(IZ) + XS
       RUNSUB   = RUNSUB - XS/DT

       if ( OPT_RUNSUB == 5 ) DEEPRECH = DEEPRECH - XS * 1.0e-3

       do IZ = 1, NSOIL
          SH2O(IZ) = MLIQ(IZ) / (DZSNSO(IZ)*1000.0)
       enddo
    endif ! OPT_RUNSUB /= 1

    ! compute groundwater and subsurface runoff
    if ( OPT_RUNSUB == 1 ) call RunoffSubSurfaceGroundWater(noahmp)

    ! compute subsurface runoff based on drainage rate
    if ( OPT_RUNSUB == 3 .or. OPT_RUNSUB == 4 .or. OPT_RUNSUB == 6 .or. &
         OPT_RUNSUB == 7 .or. OPT_RUNSUB == 8) then
         call RunoffSubSurfaceDrainage(noahmp)
    endif

    ! update soil moisture
    do IZ = 1, NSOIL
        SMC(IZ) = SH2O(IZ) + SICE(IZ)
    enddo

    ! compute subsurface runoff and shallow water table for MMF scheme
    if ( OPT_RUNSUB == 5 ) call RunoffSubSurfaceShallowWaterMMF(noahmp)

    end associate

  end subroutine SoilWaterMain

end module SoilWaterMainMod
