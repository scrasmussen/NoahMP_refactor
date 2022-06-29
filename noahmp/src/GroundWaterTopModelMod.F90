module GroundWaterTopModelMod

!!! Compute groundwater flow and subsurface runoff based on TOPMODEL (Niu et al., 2007)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundWaterTopModel(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROUNDWATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ           ! do-loop index
    integer                :: IWT          ! layer index above water table layer
    real(kind=8)           :: S_NODE       ! degree of saturation of IWT layer
    real(kind=kind_noahmp) :: DZSUM        ! cumulative depth above water table [m]
    real(kind=kind_noahmp) :: SMPFZ        ! matric potential (frozen effects) [mm]
    real(kind=kind_noahmp) :: KA           ! aquifer hydraulic conductivity [mm/s]
    real(kind=kind_noahmp) :: WH_ZWT       ! water head at water table [mm]
    real(kind=kind_noahmp) :: WH           ! water head at layer above ZWT [mm]
    real(kind=kind_noahmp) :: WS           ! water used to fill air pore [mm]
    real(kind=kind_noahmp) :: WTSUB        ! sum of HK*DZMM
    real(kind=kind_noahmp) :: WATMIN       ! minimum soil vol soil moisture [m3/m3]
    real(kind=kind_noahmp) :: XS           ! excessive water above saturation [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZMM      ! layer thickness [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZNODE     ! node depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MLIQ      ! liquid water mass [kg/m2 or mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: EPORE     ! effective porosity [-]
    real(kind=kind_noahmp), allocatable, dimension(:) :: HK        ! hydraulic conductivity [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMC       ! total soil water  content [m3/m3]

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of soil layer-bottom [m]
              FCRMAX          => noahmp%water%state%FCRMAX           ,& ! in,     maximum fraction of imperviousness (FCR)
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              WCND            => noahmp%water%state%WCND             ,& ! in,     soil hydraulic conductivity (m/s)
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              TIMEAN          => noahmp%water%param%TIMEAN           ,& ! in,     gridcell mean topgraphic index (global mean)
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              ROUS            => noahmp%water%param%ROUS             ,& ! in,     specific yield [-], default:0.2
              CMIC            => noahmp%water%param%CMIC             ,& ! in,     microprore content (0.0-1.0), default:0.2
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              ZWT             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
              WA              => noahmp%water%state%WA               ,& ! inout,  water storage in aquifer [mm]
              WT              => noahmp%water%state%WT               ,& ! inout,  water storage in aquifer + saturated soil [mm]
              FFF             => noahmp%water%param%FFF              ,& ! inout,  runoff decay factor (m-1)
              RSBMX           => noahmp%water%param%RSBMX            ,& ! inout,  baseflow coefficient [mm/s]
              QIN             => noahmp%water%flux%QIN               ,& ! out,    groundwater recharge [mm/s]
              QDIS            => noahmp%water%flux%QDIS               & ! out,    groundwater discharge [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( ZNODE (1:NSOIL) )
    allocate( DZMM  (1:NSOIL) )
    allocate( MLIQ  (1:NSOIL) )
    allocate( EPORE (1:NSOIL) )
    allocate( HK    (1:NSOIL) )
    allocate( SMC   (1:NSOIL) )
    QDIS = 0.0
    QIN  = 0.0

    ! Derive layer-bottom depth in [mm]; KWM:Derive layer thickness in mm
    DZMM(1) = -ZSOIL(1) * 1.0e3
    do IZ = 2, NSOIL
       DZMM(IZ)  = 1.0e3 * ( ZSOIL(IZ-1) - ZSOIL(IZ) )
    enddo

    ! Derive node (middle) depth in [m]; KWM: Positive number, depth below ground surface in m
    ZNODE(1) = -ZSOIL(1) / 2.0
    do IZ = 2, NSOIL
       ZNODE(IZ) = -ZSOIL(IZ-1) + 0.5 * ( ZSOIL(IZ-1) - ZSOIL(IZ) )
    enddo

    ! Convert volumetric soil moisture "sh2o" to mass
    do IZ = 1, NSOIL
       SMC(IZ)   = SH2O(IZ) + SICE(IZ)
       MLIQ(IZ)  = SH2O(IZ) * DZMM(IZ)
       EPORE(IZ) = max( 0.01, SMCMAX(IZ)-SICE(IZ) )
       HK(IZ)    = 1.0e3 * WCND(IZ)
    enddo

    ! The layer index of the first unsaturated layer (the layer right above the water table)
    IWT = NSOIL
    do IZ = 2, NSOIL
       if ( ZWT <= -ZSOIL(IZ) ) then
          IWT = IZ - 1
          exit
       endif
    enddo

    ! Groundwater discharge [mm/s]
    FFF   = 6.0
    RSBMX = 5.0
    QDIS = (1.0-FCRMAX) * RSBMX * exp(-TIMEAN) * exp(-FFF * (ZWT-2.0))

    ! Matric potential at the layer above the water table
    S_NODE = min( 1.0, SMC(IWT)/SMCMAX(IWT) )
    S_NODE = max( S_NODE, real(0.01,kind=8) )
    SMPFZ  = -PSISAT(IWT) * 1000.0 * S_NODE**(-BEXP(IWT))   ! m -> mm
    SMPFZ  = max( -120000.0, CMIC*SMPFZ )

    ! Recharge rate qin to groundwater
    KA      = HK(IWT)
    WH_ZWT  = -ZWT * 1.0e3                 !(mm)
    WH      = SMPFZ - ZNODE(IWT) * 1.0e3   !(mm)
    QIN     = -KA * (WH_ZWT - WH) / ( (ZWT-ZNODE(IWT)) * 1.0e3 )
    QIN     = max( -10.0/DT, min(10.0/DT, QIN) )

    ! Water storage in the aquifer + saturated soil
    WT  = WT + (QIN - QDIS) * DT     !(mm)
    if ( IWT == NSOIL ) then
       WA          = WA + (QIN - QDIS) * DT     !(mm)
       WT          = WA
       ZWT         = (-ZSOIL(NSOIL) + 25.0) - WA / 1000.0 / ROUS      !(m)
       MLIQ(NSOIL) = MLIQ(NSOIL) - QIN * DT        ! [mm]
       MLIQ(NSOIL) = MLIQ(NSOIL) + max( 0.0, (WA - 5000.0) )
       WA          = min( WA, 5000.0 )
    else
       if ( IWT == NSOIL-1 ) then
          ZWT = -ZSOIL(NSOIL) - (WT - ROUS*1000.0*25.0) / (EPORE(NSOIL))/1000.0
       else
          WS = 0.0   ! water used to fill soil air pores
          do IZ = IWT+2, NSOIL
             WS = WS + EPORE(IZ) * DZMM(IZ)
          enddo
          ZWT = -ZSOIL(IWT+1) - (WT - ROUS*1000.0*25.0 - WS) / (EPORE(IWT+1))/1000.0
       endif
       WTSUB = 0.0
       do IZ = 1, NSOIL
          WTSUB = WTSUB + HK(IZ) * DZMM(IZ)
       enddo
       do IZ = 1, NSOIL           ! Removing subsurface runoff
          MLIQ(IZ) = MLIQ(IZ) - QDIS * DT * HK(IZ) * DZMM(IZ) / WTSUB
       enddo
    endif

    ZWT = max( 1.5, ZWT )

    ! Limit MLIQ to be greater than or equal to watmin.
    ! Get water needed to bring MLIQ equal WATMIN from lower layer.
    WATMIN = 0.01
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
    WA       = WA - XS
    WT       = WT - XS

    ! update soil moisture
    do IZ = 1, NSOIL
        SH2O(IZ) = MLIQ(IZ) / DZMM(IZ)
    enddo

    end associate

  end subroutine GroundWaterTopModel

end module GroundWaterTopModelMod
