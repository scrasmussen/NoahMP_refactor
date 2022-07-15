module TileDrainageHooghoudtMod

!!! Calculate tile drainage discharge (mm) based on Hooghoudt's equation

  use Machine
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
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilLiqWaterAftDrain  ! remaining water after tile drain

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,    number of soil layers
              DepthSoilLayer     => noahmp%config%domain%DepthSoilLayer     ,& ! in,    depth [m] of layer-bottom from soil surface
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              GridSize        => noahmp%config%domain%GridSize       ,& ! in,    noahmp model grid spacing (m)
              ThicknessSoilLayer => noahmp%config%domain%ThicknessSoilLayer ,& ! in,    soil layer thickness [m]
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,    reference soil moisture (field capacity) (m3/m3)
              TD_DCOEF        => noahmp%water%param%TD_DCOEF         ,& ! in,    drainage coefficent (m/day)
              TD_ADEPTH       => noahmp%water%param%TD_ADEPTH        ,& ! in,    Actual depth to impermeable layer from surface (m)
              KLAT_FAC        => noahmp%water%param%KLAT_FAC         ,& ! in,    multiplication factor to determine lateral hydraulic conductivity
              TD_DDRAIN       => noahmp%water%param%TD_DDRAIN        ,& ! in,    Depth of drain (m)
              TD_SPAC         => noahmp%water%param%TD_SPAC          ,& ! in,    distance between two drain tubes or tiles (m)
              TD_RADI         => noahmp%water%param%TD_RADI          ,& ! in,    effective radius of drains (m)
              SoilWatConductivity            => noahmp%water%state%SoilWatConductivity             ,& ! in,    soil hydraulic conductivity [m/s]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,    soil ice content [m3/m3]
              WaterTableHydro         => noahmp%water%state%WaterTableHydro          ,& ! in,    water table depth estimated in WRF-Hydro fine grids (m)
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilMoisture    => noahmp%water%state%SoilMoisture    ,& ! inout, total soil moisture [m3/m3]
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! inout, water table depth [m]
              QTLDRN          => noahmp%water%flux%QTLDRN             & ! inout, tile drainage (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( TD_SATZ(1:NumSoilLayer) )
    allocate( KLATK  (1:NumSoilLayer) )
    allocate( OVRFC  (1:NumSoilLayer) )
    allocate( SoilLiqWaterAftDrain (1:NumSoilLayer) )
    TD_SATZ = 0.0
    KLATK   = 0.0
    OVRFC   = 0.0
    SoilLiqWaterAftDrain  = 0.0
    DTOPL   = 0.0
    TD_LQ   = 0.0
    TD_TTSZ = 0.0
    TDDC    = TD_DCOEF * 1000.0 * MainTimeStep / (24.0 * 3600.0) ! m per day to mm per timestep

    ! Thickness of soil layers    
    do K = 1, NumSoilLayer
       if ( K == 1 ) then
          ThicknessSoilLayer(K) = -1.0 * DepthSoilLayer(K)
       else
          ThicknessSoilLayer(K) = (DepthSoilLayer(K-1) - DepthSoilLayer(K))
       endif
    enddo

#ifdef WRF_HYDRO
    ! Depth to water table from WRF-HYDRO, m
    YY = WaterTableHydro
#else
    call WaterTableDepthSearch(noahmp)
    !call WaterTableEquilibrium(noahmp)
    YY = WaterTableDepth
#endif

    if ( YY > TD_ADEPTH) YY = TD_ADEPTH

    ! Depth of saturated zone
    do K = 1, NumSoilLayer
       if ( YY > (-1.0*DepthSoilLayer(K)) ) then
          TD_SATZ(K) = 0.0
       else
          TD_SATZ(K) = (-1.0 * DepthSoilLayer(K)) - YY
          XX         = (-1.0 * DepthSoilLayer(K)) - DTOPL
          if ( TD_SATZ(K) > XX ) TD_SATZ(K) = XX
       endif
       DTOPL = -1.0 * DepthSoilLayer(K)
    enddo

    ! amount of water over field capacity
    OVRFCS = 0.0
    do K = 1, NumSoilLayer
       OVRFC(K) = (SoilLiqWater(K) - (SMCREF(K)-SoilIce(K))) * ThicknessSoilLayer(K) * 1000.0 !mm
       if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
       OVRFCS   = OVRFCS + OVRFC(K)
    enddo

    ! lateral hydraulic conductivity and total lateral flow
    do K = 1, NumSoilLayer
       KLATK(K) = SoilWatConductivity(K) * KLAT_FAC * MainTimeStep ! m/s to m/timestep
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
    NDRAINS = int( GridSize / TD_SPAC )
    QTLDRN  = QTLDRN * NDRAINS
    if ( QTLDRN > OVRFCS ) QTLDRN = OVRFCS

    ! update soil moisture after drainage: moisture drains from top to bottom
    QTLDRN1 = QTLDRN
    do K = 1, NumSoilLayer
       if ( QTLDRN1 > 0.0) then
          if ( (TD_SATZ(K) > 0.0) .and. (OVRFC(K) > 0.0) ) then
             SoilLiqWaterAftDrain(K) = OVRFC(K) - QTLDRN1 ! remaining water after tile drain
             if ( SoilLiqWaterAftDrain(K) > 0.0 ) then
                SoilLiqWater(K) = (SMCREF(K) - SoilIce(K)) + SoilLiqWaterAftDrain(K) / (ThicknessSoilLayer(K) * 1000.0)
                SoilMoisture(K)  = SoilLiqWater(K) + SoilIce(K)
                exit
             else
                SoilLiqWater(K) = SMCREF(K) - SoilIce(K)
                SoilMoisture(K)  = SoilLiqWater(K) + SoilIce (K)
                QTLDRN1 = QTLDRN1 - OVRFC(K)
             endif
          endif
       endif
    enddo

    QTLDRN = QTLDRN / MainTimeStep ![mm/s]

    end associate

  end subroutine TileDrainageHooghoudt

end module TileDrainageHooghoudtMod
