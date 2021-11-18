module ShallowWaterTableMmfMod

!!! Diagnoses water table depth and computes recharge when the water table is 
!!! within the resolved soil layers, according to the Miguez-Macho&Fan scheme

  use Machine, only : kind_noahmp
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
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSOIL0   ! temporary soil depth

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of soil layer-bottom [m]
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
    allocate( ZSOIL0 (0:NSOIL) )
    ZSOIL0(1:NSOIL) = ZSOIL(1:NSOIL)
    ZSOIL0(0)       = 0.0

    ! find the layer where the water table is
    do IZ = NSOIL, 1, -1
       if ( WTD + 1.0e-6 < ZSOIL0(IZ) ) exit
    enddo
    IWTD = IZ

    KWTD = IWTD + 1  ! l ayer where the water table is
    if ( KWTD <= NSOIL ) then    ! wtd in the resolved layers
       WTDOLD = WTD
       if ( SMC(KWTD) > SMCEQ(KWTD) ) then
          if ( SMC(KWTD) == SMCMAX(KWTD) ) then ! wtd went to the layer above
             WTD  = ZSOIL0(IWTD)
             RECH = -(WTDOLD - WTD) * (SMCMAX(KWTD) - SMCEQ(KWTD))
             IWTD = IWTD-1
             KWTD = KWTD-1
             if ( KWTD >= 1 ) then
                if ( SMC(KWTD) > SMCEQ(KWTD) ) then
                   WTDOLD = WTD
                   WTD  = min( ( SMC(KWTD)*DZSNSO(KWTD) - SMCEQ(KWTD)*ZSOIL0(IWTD) + &
                          SMCMAX(KWTD)*ZSOIL0(KWTD) ) / (SMCMAX(KWTD)-SMCEQ(KWTD)), ZSOIL0(IWTD) )
                   RECH = RECH - (WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
                endif
             endif
          else  ! wtd stays in the layer
             WTD  = min( ( SMC(KWTD)*DZSNSO(KWTD) - SMCEQ(KWTD)*ZSOIL0(IWTD) + &
                    SMCMAX(KWTD)*ZSOIL0(KWTD) ) / (SMCMAX(KWTD)-SMCEQ(KWTD)), ZSOIL0(IWTD) )
             RECH = -(WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
          endif
       else   ! wtd has gone down to the layer below
          WTD  = ZSOIL0(KWTD)
          RECH = -(WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
          KWTD = KWTD + 1
          IWTD = IWTD + 1
          ! wtd crossed to the layer below. Now adjust it there
          if ( KWTD <= NSOIL ) then
             WTDOLD = WTD
             if ( SMC(KWTD) > SMCEQ(KWTD) ) then
                WTD = min( ( SMC(KWTD)*DZSNSO(KWTD) - SMCEQ(KWTD)*ZSOIL0(IWTD) + &
                      SMCMAX(KWTD)*ZSOIL0(KWTD) ) / (SMCMAX(KWTD)-SMCEQ(KWTD)), ZSOIL0(IWTD) )
             else
                WTD = ZSOIL0(KWTD)
             endif
             RECH = RECH - (WTDOLD-WTD) * (SMCMAX(KWTD)-SMCEQ(KWTD))
          else
             WTDOLD = WTD
             ! restore smoi to equilibrium value with water from the ficticious layer below
             ! SMCWTD=SMCWTD-(SMCEQ(NSOIL)-SMC(NSOIL))
             ! QDRAIN = QDRAIN - 1000 * (SMCEQ(NSOIL)-SMC(NSOIL)) * DZSNSO(NSOIL) / DT
             ! SMC(NSOIL)=SMCEQ(NSOIL)

             ! adjust wtd in the ficticious layer below
             SMCEQDEEP = SMCMAX(NSOIL) * ( -PSISAT(NSOIL) / (-PSISAT(NSOIL)-DZSNSO(NSOIL)) ) ** (1.0/BEXP(NSOIL))
             WTD = min( ( SMCWTD*DZSNSO(NSOIL) - SMCEQDEEP*ZSOIL0(NSOIL) + &
                   SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / (SMCMAX(NSOIL)-SMCEQDEEP), ZSOIL0(NSOIL) )
             RECH = RECH - (WTDOLD-WTD) * (SMCMAX(NSOIL)-SMCEQDEEP)
          endif
       endif
    else if ( WTD >= ZSOIL0(NSOIL)-DZSNSO(NSOIL) ) then
    ! if wtd was already below the bottom of the resolved soil crust
       WTDOLD = WTD
       SMCEQDEEP = SMCMAX(NSOIL) * ( -PSISAT(NSOIL) / (-PSISAT(NSOIL)-DZSNSO(NSOIL)) ) ** (1.0/BEXP(NSOIL))
       if ( SMCWTD > SMCEQDEEP ) then
          WTD = min( ( SMCWTD*DZSNSO(NSOIL) - SMCEQDEEP*ZSOIL0(NSOIL) + &
                SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / (SMCMAX(NSOIL)-SMCEQDEEP), ZSOIL0(NSOIL) )
          RECH = -(WTDOLD-WTD) * (SMCMAX(NSOIL)-SMCEQDEEP)
       else
          RECH = -( WTDOLD - (ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) * (SMCMAX(NSOIL) - SMCEQDEEP)
          WTDOLD = ZSOIL0(NSOIL) - DZSNSO(NSOIL)
          ! and now even further down
          DZUP = (SMCEQDEEP-SMCWTD) * DZSNSO(NSOIL) / (SMCMAX(NSOIL)-SMCEQDEEP)
          WTD  = WTDOLD - DZUP
          RECH = RECH - (SMCMAX(NSOIL)-SMCEQDEEP) * DZUP
          SMCWTD = SMCEQDEEP
       endif
    endif

    if ( IWTD < NSOIL .and. IWTD > 0 ) then
       SMCWTD = SMCMAX(IWTD)
    else if ( IWTD < NSOIL .and. IWTD <= 0) then
       SMCWTD = SMCMAX(1)
    endif

    end associate

  end subroutine ShallowWaterTableMMF

end module ShallowWaterTableMmfMod
