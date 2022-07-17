module GroundWaterTopModelMod

!!! Compute groundwater flow and subsurface runoff based on TOPMODEL (Niu et al., 2007)

  use Machine
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
    real(kind=kind_noahmp) :: WH           ! water head at layer above water table [mm]
    real(kind=kind_noahmp) :: WS           ! water used to fill air pore [mm]
    real(kind=kind_noahmp) :: WTSUB        ! sum of HK*DZMM
    real(kind=kind_noahmp) :: WATMIN       ! minimum soil vol soil moisture [m3/m3]
    real(kind=kind_noahmp) :: XS           ! excessive water above saturation [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZMM      ! layer thickness [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZNODE     ! node depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MLIQ      ! liquid water mass [kg/m2 or mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilEffPorosity     ! soil effective porosity [-]
    real(kind=kind_noahmp), allocatable, dimension(:) :: HK        ! hydraulic conductivity [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoisture       ! total soil water  content [m3/m3]

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step [s]
              DepthSoilLayer  => noahmp%config%domain%DepthSoilLayer ,& ! in,     depth of soil layer-bottom [m]
              SoilImpervFracMax          => noahmp%water%state%SoilImpervFracMax           ,& ! in,     maximum soil imperviousness fraction
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,     soil ice content [m3/m3]
              SoilWatConductivity            => noahmp%water%state%SoilWatConductivity             ,& ! in,     soil hydraulic conductivity [m/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              TIMEAN          => noahmp%water%param%TIMEAN           ,& ! in,     gridcell mean topgraphic index (global mean)
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              ROUS            => noahmp%water%param%ROUS             ,& ! in,     specific yield [-], default:0.2
              CMIC            => noahmp%water%param%CMIC             ,& ! in,     microprore content (0.0-1.0), default:0.2
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil water content [m3/m3]
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! inout,  water table depth [m]
              WaterStorageAquifer  => noahmp%water%state%WaterStorageAquifer ,& ! inout,  water storage in aquifer [mm]
              WaterStorageSoilAqf   => noahmp%water%state%WaterStorageSoilAqf ,& ! inout,  water storage in aquifer + saturated soil [mm]
              FFF             => noahmp%water%param%FFF              ,& ! inout,  runoff decay factor (m-1)
              RSBMX           => noahmp%water%param%RSBMX            ,& ! inout,  baseflow coefficient [mm/s]
              RechargeGw             => noahmp%water%flux%RechargeGw               ,& ! out,    groundwater recharge rate [mm/s]
              DischargeGw            => noahmp%water%flux%DischargeGw               & ! out,    groundwater discharge rate [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( ZNODE (1:NumSoilLayer) )
    allocate( DZMM  (1:NumSoilLayer) )
    allocate( MLIQ  (1:NumSoilLayer) )
    allocate( SoilEffPorosity (1:NumSoilLayer) )
    allocate( HK    (1:NumSoilLayer) )
    allocate( SoilMoisture   (1:NumSoilLayer) )
    DischargeGw = 0.0
    RechargeGw  = 0.0

    ! Derive layer-bottom depth in [mm]; KWM:Derive layer thickness in mm
    DZMM(1) = -DepthSoilLayer(1) * 1.0e3
    do IZ = 2, NumSoilLayer
       DZMM(IZ)  = 1.0e3 * ( DepthSoilLayer(IZ-1) - DepthSoilLayer(IZ) )
    enddo

    ! Derive node (middle) depth in [m]; KWM: Positive number, depth below ground surface in m
    ZNODE(1) = -DepthSoilLayer(1) / 2.0
    do IZ = 2, NumSoilLayer
       ZNODE(IZ) = -DepthSoilLayer(IZ-1) + 0.5 * ( DepthSoilLayer(IZ-1) - DepthSoilLayer(IZ) )
    enddo

    ! Convert volumetric soil moisture to mass
    do IZ = 1, NumSoilLayer
       SoilMoisture(IZ)   = SoilLiqWater(IZ) + SoilIce(IZ)
       MLIQ(IZ)  = SoilLiqWater(IZ) * DZMM(IZ)
       SoilEffPorosity(IZ) = max( 0.01, SMCMAX(IZ)-SoilIce(IZ) )
       HK(IZ)    = 1.0e3 * SoilWatConductivity(IZ)
    enddo

    ! The layer index of the first unsaturated layer (the layer right above the water table)
    IWT = NumSoilLayer
    do IZ = 2, NumSoilLayer
       if ( WaterTableDepth <= -DepthSoilLayer(IZ) ) then
          IWT = IZ - 1
          exit
       endif
    enddo

    ! Groundwater discharge [mm/s]
    FFF   = 6.0
    RSBMX = 5.0
    DischargeGw = (1.0-SoilImpervFracMax) * RSBMX * exp(-TIMEAN) * exp(-FFF * (WaterTableDepth-2.0))

    ! Matric potential at the layer above the water table
    S_NODE = min( 1.0, SoilMoisture(IWT)/SMCMAX(IWT) )
    S_NODE = max( S_NODE, real(0.01,kind=8) )
    SMPFZ  = -PSISAT(IWT) * 1000.0 * S_NODE**(-BEXP(IWT))   ! m -> mm
    SMPFZ  = max( -120000.0, CMIC*SMPFZ )

    ! Recharge rate qin to groundwater
    KA      = HK(IWT)
    WH_ZWT  = -WaterTableDepth * 1.0e3                 !(mm)
    WH      = SMPFZ - ZNODE(IWT) * 1.0e3   !(mm)
    RechargeGw     = -KA * (WH_ZWT - WH) / ( (WaterTableDepth-ZNODE(IWT)) * 1.0e3 )
    RechargeGw     = max( -10.0/MainTimeStep, min(10.0/MainTimeStep, RechargeGw) )

    ! Water storage in the aquifer + saturated soil
    WaterStorageSoilAqf = WaterStorageSoilAqf + (RechargeGw - DischargeGw) * MainTimeStep     !(mm)
    if ( IWT == NumSoilLayer ) then
       WaterStorageAquifer = WaterStorageAquifer + (RechargeGw - DischargeGw) * MainTimeStep     !(mm)
       WaterStorageSoilAqf = WaterStorageAquifer
       WaterTableDepth = (-DepthSoilLayer(NumSoilLayer) + 25.0) - WaterStorageAquifer / 1000.0 / ROUS      !(m)
       MLIQ(NumSoilLayer) = MLIQ(NumSoilLayer) - RechargeGw * MainTimeStep        ! [mm]
       MLIQ(NumSoilLayer) = MLIQ(NumSoilLayer) + max( 0.0, (WaterStorageAquifer - 5000.0) )
       WaterStorageAquifer = min( WaterStorageAquifer, 5000.0 )
    else
       if ( IWT == NumSoilLayer-1 ) then
          WaterTableDepth = -DepthSoilLayer(NumSoilLayer) - &
                           (WaterStorageSoilAqf - ROUS*1000.0*25.0) / (SoilEffPorosity(NumSoilLayer))/1000.0
       else
          WS = 0.0   ! water used to fill soil air pores
          do IZ = IWT+2, NumSoilLayer
             WS = WS + SoilEffPorosity(IZ) * DZMM(IZ)
          enddo
          WaterTableDepth = -DepthSoilLayer(IWT+1) - &
                           (WaterStorageSoilAqf - ROUS*1000.0*25.0 - WS) / (SoilEffPorosity(IWT+1))/1000.0
       endif
       WTSUB = 0.0
       do IZ = 1, NumSoilLayer
          WTSUB = WTSUB + HK(IZ) * DZMM(IZ)
       enddo
       do IZ = 1, NumSoilLayer           ! Removing subsurface runoff
          MLIQ(IZ) = MLIQ(IZ) - DischargeGw * MainTimeStep * HK(IZ) * DZMM(IZ) / WTSUB
       enddo
    endif

    WaterTableDepth = max( 1.5, WaterTableDepth )

    ! Limit MLIQ to be greater than or equal to watmin.
    ! Get water needed to bring MLIQ equal WATMIN from lower layer.
    WATMIN = 0.01
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
    WaterStorageAquifer = WaterStorageAquifer - XS
    WaterStorageSoilAqf = WaterStorageSoilAqf - XS

    ! update soil moisture
    do IZ = 1, NumSoilLayer
        SoilLiqWater(IZ) = MLIQ(IZ) / DZMM(IZ)
    enddo

    end associate

  end subroutine GroundWaterTopModel

end module GroundWaterTopModelMod
