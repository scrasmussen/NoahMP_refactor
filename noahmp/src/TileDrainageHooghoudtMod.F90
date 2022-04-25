module TileDrainageHooghoudtMod

!!! Calculate tile drainage discharge (mm) based on Hooghoudt's equation

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use TileDrainageEquiDepthMod, only : TileDrainageEquiDepth
  use WaterTableDepthSearchMod, only : WaterTableDepthSearch
  use WaterTableEquilibriumMod, only : WaterTableEquilibrium

  implicit none

contains

  subroutine TileDrainageHooghoudt(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TILE_HOOGHOUDT
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: K         ! loop index 
    integer                :: NDRAINS   ! number of drains
    real(kind=kind_noahmp) :: TD_TTSZ   ! Total Thickness of Saturated Zone
    real(kind=kind_noahmp) :: TD_LQ     ! lateral flow
    real(kind=kind_noahmp) :: DTOPL     ! depth to top of the layer
    real(kind=kind_noahmp) :: XX        ! temporary water table variable
    real(kind=kind_noahmp) :: YY        ! temporary water table variable
    real(kind=kind_noahmp) :: KLAT      ! average lateral hydruaic conductivity
    real(kind=kind_noahmp) :: TD_HAIL   ! Height of water table in the drain Above Impermeable Layer
    real(kind=kind_noahmp) :: TD_DEPTH  ! Effective Depth to impermeable layer from soil surface
    real(kind=kind_noahmp) :: TD_HEMD   ! Effective Height between water level in the drains to the water table MiDpoint
    real(kind=kind_noahmp) :: TDDC      ! Drainage Coefficient
    real(kind=kind_noahmp) :: QTLDRN1   ! temporary drainage discharge
    real(kind=kind_noahmp) :: TD_DD     ! drain depth to impermeable layer
    real(kind=kind_noahmp) :: OVRFCS    ! amount of water over field capacity
    real(kind=kind_noahmp), allocatable, dimension(:) :: TD_SATZ ! thickness of saturated zone
    real(kind=kind_noahmp), allocatable, dimension(:) :: KLATK   ! lateral hydraulic ocnductivity kth layer
    real(kind=kind_noahmp), allocatable, dimension(:) :: OVRFC   ! layer-wise amount of water over field capacity
    real(kind=kind_noahmp), allocatable, dimension(:) :: RMSH2O  ! remaining water after tile drain

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface (m)
              DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
              DX              => noahmp%config%domain%DX             ,& ! in,     noahmp model grid spacing (m)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              ZLAYER          => noahmp%config%domain%ZLAYER         ,& ! in,     soil layer thickness (m)
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,     reference soil moisture (field capacity) (m3/m3)
              TD_DCOEF        => noahmp%water%param%TD_DCOEF         ,& ! in,     drainage coefficent (m/day)
              TD_ADEPTH       => noahmp%water%param%TD_ADEPTH        ,& ! in,     Actual depth to impermeable layer from surface (m)
              KLAT_FAC        => noahmp%water%param%KLAT_FAC         ,& ! in,     multiplication factor to determine lateral hydraulic conductivity
              TD_DDRAIN       => noahmp%water%param%TD_DDRAIN        ,& ! in,     Depth of drain (m)
              TD_SPAC         => noahmp%water%param%TD_SPAC          ,& ! in,     distance between two drain tubes or tiles (m)
              TD_RADI         => noahmp%water%param%TD_RADI          ,& ! in,     effective radius of drains (m)
              WCND            => noahmp%water%state%WCND             ,& ! in,     soil hydraulic conductivity (m/s)
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              WATBLED         => noahmp%water%state%WATBLED          ,& ! in,     water table depth estimated in WRF-Hydro fine grids (m)
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              ZWT             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
              QTLDRN          => noahmp%water%flux%QTLDRN             & ! inout,  tile drainage (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( TD_SATZ(1:NSOIL) )
    allocate( KLATK  (1:NSOIL) )
    allocate( OVRFC  (1:NSOIL) )
    allocate( RMSH2O (1:NSOIL) )
    TD_SATZ = 0.0
    KLATK   = 0.0
    OVRFC   = 0.0
    RMSH2O  = 0.0
    DTOPL   = 0.0
    TD_LQ   = 0.0
    TD_TTSZ = 0.0
    TDDC    = TD_DCOEF * 1000.0 * DT / (24.0 * 3600.0) ! m per day to mm per timestep

    ! Thickness of soil layers    
    do K = 1, NSOIL
       if ( K == 1 ) then
          ZLAYER(K) = -1.0 * ZSOIL(K)
       else
          ZLAYER(K) = (ZSOIL(K-1) - ZSOIL(K))
       endif
    enddo

#ifdef WRF_HYDRO
    ! Depth to water table from WRF-HYDRO, m
    YY = WATBLED
#else
    call WaterTableDepthSearch(noahmp)
    !call WaterTableEquilibrium(noahmp)
    YY = ZWT
#endif

    if ( YY > TD_ADEPTH) YY = TD_ADEPTH

    ! Depth of saturated zone
    do K = 1, NSOIL
       if ( YY > (-1.0*ZSOIL(K)) ) then
          TD_SATZ(K) = 0.0
       else
          TD_SATZ(K) = (-1.0 * ZSOIL(K)) - YY
          XX         = (-1.0 * ZSOIL(K)) - DTOPL
          if ( TD_SATZ(K) > XX ) TD_SATZ(K) = XX
       endif
       DTOPL = -1.0 * ZSOIL(K)
    enddo

    ! amount of water over field capacity
    OVRFCS = 0.0
    do K = 1, NSOIL
       OVRFC(K) = (SH2O(K) - (SMCREF(K)-SICE(K))) * ZLAYER(K) * 1000.0 !mm
       if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
       OVRFCS   = OVRFCS + OVRFC(K)
    enddo

    ! lateral hydraulic conductivity and total lateral flow
    do K = 1, NSOIL
       KLATK(K) = WCND(K) * KLAT_FAC * DT ! m/s to m/timestep
       TD_LQ    = TD_LQ + (TD_SATZ(K) * KLATK(K))
       TD_TTSZ  = TD_TTSZ + TD_SATZ(K)
    enddo
    if ( TD_TTSZ < 0.001 ) TD_TTSZ = 0.001 ! unit is m
    if ( TD_LQ   < 0.001 ) TD_LQ   = 0.0    ! unit is m
    KLAT  = TD_LQ / TD_TTSZ ! lateral hydraulic conductivity per timestep
    TD_DD = TD_ADEPTH - TD_DDRAIN

    call TileDrainageEquiDepth(TD_DD, TD_SPAC, TD_RADI, TD_HAIL)

    TD_DEPTH = TD_HAIL + TD_DDRAIN
    TD_HEMD  = TD_DDRAIN - YY
    if ( TD_HEMD <= 0.0 ) then
       QTLDRN = 0.0
    else
       QTLDRN = ( (8.0*KLAT*TD_HAIL*TD_HEMD) + (4.0*KLAT*TD_HEMD*TD_HEMD) ) & ! m per timestep
                                                        / (TD_SPAC*TD_SPAC)
    endif
    QTLDRN = QTLDRN * 1000.0 ! m per timestep to mm/timestep /one tile
    if ( QTLDRN <= 0.0 ) QTLDRN = 0.0
    if ( QTLDRN > TDDC ) QTLDRN = TDDC
    NDRAINS = int( DX / TD_SPAC )
    QTLDRN  = QTLDRN * NDRAINS
    if ( QTLDRN > OVRFCS ) QTLDRN = OVRFCS

    ! update soil moisture after drainage: moisture drains from top to bottom
    QTLDRN1 = QTLDRN
    do K = 1, NSOIL
       if ( QTLDRN1 > 0.0) then
          if ( (TD_SATZ(K) > 0.0) .and. (OVRFC(K) > 0.0) ) then
             RMSH2O(K) = OVRFC(K) - QTLDRN1 ! remaining water after tile drain
             if ( RMSH2O(K) > 0.0 ) then
                SH2O(K) = (SMCREF(K) - SICE(K)) + RMSH2O(K) / (ZLAYER(K) * 1000.0)
                SMC(K)  = SH2O(K) + SICE(K)
                exit
             else
                SH2O(K) = SMCREF(K) - SICE(K)
                SMC(K)  = SH2O(K) + SICE (K)
                QTLDRN1 = QTLDRN1 - OVRFC(K)
             endif
          endif
       endif
    enddo

    QTLDRN = QTLDRN / DT ![mm/s]

    end associate

  end subroutine TileDrainageHooghoudt

end module TileDrainageHooghoudtMod
