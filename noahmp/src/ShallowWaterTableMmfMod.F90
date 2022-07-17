module ShallowWaterTableMmfMod

!!! Diagnoses water table depth and computes recharge when the water table is 
!!! within the resolved soil layers, according to the Miguez-Macho&Fan scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ShallowWaterTableMMF(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SHALLOWWATERTABLE
! Original code: Miguez-Macho&Fan (Miguez-Macho et al 2007, Fan et al 2007)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ        ! do-loop index
    integer                :: IWTD      ! layer index above water table layer
    integer                :: KWTD      ! layer index where the water table layer is
    real(kind=kind_noahmp) :: WTDOLD    ! old water table depth
    real(kind=kind_noahmp) :: DZUP      ! upper layer thickness
    real(kind=kind_noahmp) :: SMCEQDEEP ! deep layer soil moisture
    real(kind=kind_noahmp), allocatable, dimension(:) :: DepthSoilLayer0   ! temporary soil depth

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,     depth of soil layer-bottom [m]
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,     thickness of snow/soil layers (m)
              SoilMoistureEqui           => noahmp%water%state%SoilMoistureEqui  ,& ! in,     equilibrium soil water  content [m3/m3]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              SoilMoisture             => noahmp%water%state%SoilMoisture             ,& ! inout,  total soil water content [m3/m3]
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! inout,  water table depth [m]
              SoilMoistureToWT          => noahmp%water%state%SoilMoistureToWT           ,& ! inout,  soil moisture between bottom of the soil and the water table
              DrainSoilBot          => noahmp%water%flux%DrainSoilBot            ,& ! inout,  soil bottom drainage (m/s)
              RechargeGwShallowWT            => noahmp%water%state%RechargeGwShallowWT              & ! out,    groundwater recharge (net vertical flux across the water table), positive up
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( DepthSoilLayer0 (0:NumSoilLayer) )
    DepthSoilLayer0(1:NumSoilLayer) = DepthSoilLayer(1:NumSoilLayer)
    DepthSoilLayer0(0)       = 0.0

    ! find the layer where the water table is
    do IZ = NumSoilLayer, 1, -1
       if ( (WaterTableDepth + 1.0e-6) < DepthSoilLayer0(IZ) ) exit
    enddo
    IWTD = IZ

    KWTD = IWTD + 1  ! l ayer where the water table is
    if ( KWTD <= NumSoilLayer ) then    ! wtd in the resolved layers
       WTDOLD = WaterTableDepth
       if ( SoilMoisture(KWTD) > SoilMoistureEqui(KWTD) ) then
          if ( SoilMoisture(KWTD) == SMCMAX(KWTD) ) then ! wtd went to the layer above
             WaterTableDepth  = DepthSoilLayer0(IWTD)
             RechargeGwShallowWT = -(WTDOLD - WaterTableDepth) * (SMCMAX(KWTD) - SoilMoistureEqui(KWTD))
             IWTD = IWTD-1
             KWTD = KWTD-1
             if ( KWTD >= 1 ) then
                if ( SoilMoisture(KWTD) > SoilMoistureEqui(KWTD) ) then
                   WTDOLD = WaterTableDepth
                   WaterTableDepth  = min( ( SoilMoisture(KWTD)*ThicknessSnowSoilLayer(KWTD) - SoilMoistureEqui(KWTD)*DepthSoilLayer0(IWTD) + &
                          SMCMAX(KWTD)*DepthSoilLayer0(KWTD) ) / (SMCMAX(KWTD)-SoilMoistureEqui(KWTD)), DepthSoilLayer0(IWTD) )
                   RechargeGwShallowWT = RechargeGwShallowWT - (WTDOLD-WaterTableDepth) * (SMCMAX(KWTD)-SoilMoistureEqui(KWTD))
                endif
             endif
          else  ! wtd stays in the layer
             WaterTableDepth  = min( ( SoilMoisture(KWTD)*ThicknessSnowSoilLayer(KWTD) - SoilMoistureEqui(KWTD)*DepthSoilLayer0(IWTD) + &
                    SMCMAX(KWTD)*DepthSoilLayer0(KWTD) ) / (SMCMAX(KWTD)-SoilMoistureEqui(KWTD)), DepthSoilLayer0(IWTD) )
             RechargeGwShallowWT = -(WTDOLD-WaterTableDepth) * (SMCMAX(KWTD)-SoilMoistureEqui(KWTD))
          endif
       else   ! wtd has gone down to the layer below
          WaterTableDepth  = DepthSoilLayer0(KWTD)
          RechargeGwShallowWT = -(WTDOLD-WaterTableDepth) * (SMCMAX(KWTD)-SoilMoistureEqui(KWTD))
          KWTD = KWTD + 1
          IWTD = IWTD + 1
          ! wtd crossed to the layer below. Now adjust it there
          if ( KWTD <= NumSoilLayer ) then
             WTDOLD = WaterTableDepth
             if ( SoilMoisture(KWTD) > SoilMoistureEqui(KWTD) ) then
                WaterTableDepth = min( ( SoilMoisture(KWTD)*ThicknessSnowSoilLayer(KWTD) - SoilMoistureEqui(KWTD)*DepthSoilLayer0(IWTD) + &
                      SMCMAX(KWTD)*DepthSoilLayer0(KWTD) ) / (SMCMAX(KWTD)-SoilMoistureEqui(KWTD)), DepthSoilLayer0(IWTD) )
             else
                WaterTableDepth = DepthSoilLayer0(KWTD)
             endif
             RechargeGwShallowWT = RechargeGwShallowWT - (WTDOLD-WaterTableDepth) * (SMCMAX(KWTD)-SoilMoistureEqui(KWTD))
          else
             WTDOLD = WaterTableDepth
             ! restore smoi to equilibrium value with water from the ficticious layer below
             ! SoilMoistureToWT=SoilMoistureToWT-(SoilMoistureEqui(NumSoilLayer)-SoilMoisture(NumSoilLayer))
             ! DrainSoilBot = DrainSoilBot - 1000 * (SoilMoistureEqui(NumSoilLayer)-SoilMoisture(NumSoilLayer)) * ThicknessSnowSoilLayer(NumSoilLayer) / MainTimeStep
             ! SoilMoisture(NumSoilLayer)=SoilMoistureEqui(NumSoilLayer)

             ! adjust wtd in the ficticious layer below
             SMCEQDEEP = SMCMAX(NumSoilLayer) * ( -PSISAT(NumSoilLayer) / &
                         (-PSISAT(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)))**(1.0/BEXP(NumSoilLayer))
             WaterTableDepth = min( (SoilMoistureToWT*ThicknessSnowSoilLayer(NumSoilLayer) - SMCEQDEEP*DepthSoilLayer0(NumSoilLayer) + &
                         SMCMAX(NumSoilLayer)*(DepthSoilLayer0(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer))) / &
                        (SMCMAX(NumSoilLayer)-SMCEQDEEP),DepthSoilLayer0(NumSoilLayer))
             RechargeGwShallowWT = RechargeGwShallowWT - (WTDOLD-WaterTableDepth) * (SMCMAX(NumSoilLayer)-SMCEQDEEP)
          endif
       endif
    else if ( WaterTableDepth >= (DepthSoilLayer0(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)) ) then
    ! if wtd was already below the bottom of the resolved soil crust
       WTDOLD = WaterTableDepth
       SMCEQDEEP = SMCMAX(NumSoilLayer) * ( -PSISAT(NumSoilLayer) / &
                   (-PSISAT(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)) )**(1.0/BEXP(NumSoilLayer))
       if ( SoilMoistureToWT > SMCEQDEEP ) then
          WaterTableDepth = min( (SoilMoistureToWT*ThicknessSnowSoilLayer(NumSoilLayer) - SMCEQDEEP*DepthSoilLayer0(NumSoilLayer) + &
                      SMCMAX(NumSoilLayer)*(DepthSoilLayer0(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer))) / &
                    (SMCMAX(NumSoilLayer)-SMCEQDEEP), DepthSoilLayer0(NumSoilLayer) )
          RechargeGwShallowWT = -(WTDOLD-WaterTableDepth) * (SMCMAX(NumSoilLayer)-SMCEQDEEP)
       else
          RechargeGwShallowWT = -( WTDOLD - (DepthSoilLayer0(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)) ) * &
                  (SMCMAX(NumSoilLayer) - SMCEQDEEP)
          WTDOLD = DepthSoilLayer0(NumSoilLayer) - ThicknessSnowSoilLayer(NumSoilLayer)
          ! and now even further down
          DZUP = (SMCEQDEEP-SoilMoistureToWT) * ThicknessSnowSoilLayer(NumSoilLayer) / (SMCMAX(NumSoilLayer)-SMCEQDEEP)
          WaterTableDepth  = WTDOLD - DZUP
          RechargeGwShallowWT = RechargeGwShallowWT - (SMCMAX(NumSoilLayer)-SMCEQDEEP) * DZUP
          SoilMoistureToWT = SMCEQDEEP
       endif
    endif

    if ( (IWTD < NumSoilLayer) .and. (IWTD > 0) ) then
       SoilMoistureToWT = SMCMAX(IWTD)
    else if ( (IWTD < NumSoilLayer) .and. (IWTD <= 0) ) then
       SoilMoistureToWT = SMCMAX(1)
    endif

    end associate

  end subroutine ShallowWaterTableMMF

end module ShallowWaterTableMmfMod
