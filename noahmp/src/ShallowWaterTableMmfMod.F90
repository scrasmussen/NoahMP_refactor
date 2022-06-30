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
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              SMCEQ           => noahmp%water%state%SMCEQ            ,& ! in,     equilibrium soil water  content [m3/m3]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil water content [m3/m3]
              WTD             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
              SMCWTD          => noahmp%water%state%SMCWTD           ,& ! inout,  soil moisture between bottom of the soil and the water table
              QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! inout,  soil bottom drainage (m/s)
              RECH            => noahmp%water%state%RECH              & ! out,    groundwater recharge (net vertical flux across the water table), positive up
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( DepthSoilLayer0 (0:NumSoilLayer) )
    DepthSoilLayer0(1:NumSoilLayer) = DepthSoilLayer(1:NumSoilLayer)
    DepthSoilLayer0(0)       = 0.0

    ! find the layer where the water table is
    do IZ = NumSoilLayer, 1, -1
       if ( (WTD + 1.0e-6) < DepthSoilLayer0(IZ) ) exit
    enddo
    IWTD = IZ

    KWTD = IWTD + 1  ! l ayer where the water table is
    if ( KWTD <= NumSoilLayer ) then    ! wtd in the resolved layers
       WTDOLD = WTD
       if ( SMC(KWTD) > SMCEQ(KWTD) ) then
          if ( SMC(KWTD) == SMCMAX(KWTD) ) then ! wtd went to the layer above
             WTD  = DepthSoilLayer0(IWTD)
             RECH = -(WTDOLD - WTD) * (SMCMAX(KWTD) - SMCEQ(KWTD))
             IWTD = IWTD-1
             KWTD = KWTD-1
             if ( KWTD >= 1 ) then
                if ( SMC(KWTD) > SMCEQ(KWTD) ) then
                   WTDOLD = WTD
                   WTD  = min( ( SMC(KWTD)*DZSNSO(KWTD) - SMCEQ(KWTD)*DepthSoilLayer0(IWTD) + &
                          SMCMAX(KWTD)*DepthSoilLayer0(KWTD) ) / (SMCMAX(KWTD)-SMCEQ(KWTD)), DepthSoilLayer0(IWTD) )
                   RECH = RECH - (WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
                endif
             endif
          else  ! wtd stays in the layer
             WTD  = min( ( SMC(KWTD)*DZSNSO(KWTD) - SMCEQ(KWTD)*DepthSoilLayer0(IWTD) + &
                    SMCMAX(KWTD)*DepthSoilLayer0(KWTD) ) / (SMCMAX(KWTD)-SMCEQ(KWTD)), DepthSoilLayer0(IWTD) )
             RECH = -(WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
          endif
       else   ! wtd has gone down to the layer below
          WTD  = DepthSoilLayer0(KWTD)
          RECH = -(WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
          KWTD = KWTD + 1
          IWTD = IWTD + 1
          ! wtd crossed to the layer below. Now adjust it there
          if ( KWTD <= NumSoilLayer ) then
             WTDOLD = WTD
             if ( SMC(KWTD) > SMCEQ(KWTD) ) then
                WTD = min( ( SMC(KWTD)*DZSNSO(KWTD) - SMCEQ(KWTD)*DepthSoilLayer0(IWTD) + &
                      SMCMAX(KWTD)*DepthSoilLayer0(KWTD) ) / (SMCMAX(KWTD)-SMCEQ(KWTD)), DepthSoilLayer0(IWTD) )
             else
                WTD = DepthSoilLayer0(KWTD)
             endif
             RECH = RECH - (WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
          else
             WTDOLD = WTD
             ! restore smoi to equilibrium value with water from the ficticious layer below
             ! SMCWTD=SMCWTD-(SMCEQ(NumSoilLayer)-SMC(NumSoilLayer))
             ! QDRAIN = QDRAIN - 1000 * (SMCEQ(NumSoilLayer)-SMC(NumSoilLayer)) * DZSNSO(NumSoilLayer) / MainTimeStep
             ! SMC(NumSoilLayer)=SMCEQ(NumSoilLayer)

             ! adjust wtd in the ficticious layer below
             SMCEQDEEP = SMCMAX(NumSoilLayer) * &
                        (-PSISAT(NumSoilLayer)/(-PSISAT(NumSoilLayer)-DZSNSO(NumSoilLayer)))**(1.0/BEXP(NumSoilLayer))
             WTD = min( (SMCWTD*DZSNSO(NumSoilLayer) - SMCEQDEEP*DepthSoilLayer0(NumSoilLayer) + &
                         SMCMAX(NumSoilLayer)*(DepthSoilLayer0(NumSoilLayer)-DZSNSO(NumSoilLayer))) / &
                        (SMCMAX(NumSoilLayer)-SMCEQDEEP),DepthSoilLayer0(NumSoilLayer))
             RECH = RECH - (WTDOLD-WTD) * (SMCMAX(NumSoilLayer)-SMCEQDEEP)
          endif
       endif
    else if ( WTD >= (DepthSoilLayer0(NumSoilLayer)-DZSNSO(NumSoilLayer)) ) then
    ! if wtd was already below the bottom of the resolved soil crust
       WTDOLD = WTD
       SMCEQDEEP = SMCMAX(NumSoilLayer) * ( -PSISAT(NumSoilLayer) / &
                   (-PSISAT(NumSoilLayer)-DZSNSO(NumSoilLayer)) )**(1.0/BEXP(NumSoilLayer))
       if ( SMCWTD > SMCEQDEEP ) then
          WTD = min( (SMCWTD*DZSNSO(NumSoilLayer) - SMCEQDEEP*DepthSoilLayer0(NumSoilLayer) + &
                      SMCMAX(NumSoilLayer)*(DepthSoilLayer0(NumSoilLayer)-DZSNSO(NumSoilLayer))) / &
                    (SMCMAX(NumSoilLayer)-SMCEQDEEP), DepthSoilLayer0(NumSoilLayer) )
          RECH = -(WTDOLD-WTD) * (SMCMAX(NumSoilLayer)-SMCEQDEEP)
       else
          RECH = -( WTDOLD - (DepthSoilLayer0(NumSoilLayer)-DZSNSO(NumSoilLayer)) ) * (SMCMAX(NumSoilLayer) - SMCEQDEEP)
          WTDOLD = DepthSoilLayer0(NumSoilLayer) - DZSNSO(NumSoilLayer)
          ! and now even further down
          DZUP = (SMCEQDEEP-SMCWTD) * DZSNSO(NumSoilLayer) / (SMCMAX(NumSoilLayer)-SMCEQDEEP)
          WTD  = WTDOLD - DZUP
          RECH = RECH - (SMCMAX(NumSoilLayer)-SMCEQDEEP) * DZUP
          SMCWTD = SMCEQDEEP
       endif
    endif

    if ( (IWTD < NumSoilLayer) .and. (IWTD > 0) ) then
       SMCWTD = SMCMAX(IWTD)
    else if ( (IWTD < NumSoilLayer) .and. (IWTD <= 0) ) then
       SMCWTD = SMCMAX(1)
    endif

    end associate

  end subroutine ShallowWaterTableMMF

end module ShallowWaterTableMmfMod
