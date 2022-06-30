module RunoffSurfaceVicMod

!!! Compute saturated area, surface infiltration, and surface runoff based on VIC runoff scheme
!!! This scheme is adopted from VIC model

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceVIC(noahmp, DT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: COMPUTE_VIC_SURFRUNOFF
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)

! local variable
    integer                :: IZ                         ! do-loop index
    real(kind=kind_noahmp) :: EX, I_0, I_MAX, BASIS      ! temporary infiltration variable
    real(kind=kind_noahmp) :: TOP_MOIST, TOP_MAX_MOIST   ! top layer soil moisture

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              DepthSoilLayer  => noahmp%config%domain%DepthSoilLayer ,& ! in,     depth [m] of layer-bottom from soil surface
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              BVIC            => noahmp%water%param%BVIC             ,& ! in,     VIC model infiltration parameter
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              PDDUM           => noahmp%water%flux%PDDUM             ,& ! out,    infiltration rate at surface (mm/s)
              ASAT            => noahmp%water%state%FSAT              & ! out,    fractional saturated area for soil moisture
             )
! ----------------------------------------------------------------------

    ! Initialization
    EX            = 0.0
    ASAT          = 0.0
    I_MAX         = 0.0
    I_0           = 0.0
    BASIS         = 0.0
    TOP_MOIST     = 0.0
    TOP_MAX_MOIST = 0.0
    RUNSRF        = 0.0
    PDDUM         = 0.0

    do IZ = 1, NumSoilLayer-2
       TOP_MOIST     = TOP_MOIST + SMC(IZ) * (-1.0) * DepthSoilLayer(IZ)  ! m
       TOP_MAX_MOIST = TOP_MAX_MOIST + SMCMAX(IZ) * (-1.0) * DepthSoilLayer(IZ) ! m  
    enddo

    ! fractional saturated area from soil moisture
    EX    = BVIC / ( 1.0 + BVIC )
    ASAT  = 1.0 - ( max( 0.0, (1.0-(TOP_MOIST/TOP_MAX_MOIST))))**EX
    ASAT  = max(0.0, ASAT)
    ASAT  = min(1.0, ASAT)

    ! Infiltration for the previous time-step soil moisture based on ASAT
    I_MAX = (1.0 + BVIC) * TOP_MAX_MOIST ! m
    I_0   = I_MAX * ( 1.0 - (1.0 - ASAT)**(1.0/BVIC) ) !m

    ! Solve for surface runoff
    if ( QINSUR == 0.0 ) then
       RUNSRF = 0.0
    else if ( I_MAX == 0.0 ) then
       RUNSRF = QINSUR*DT
    else if ( (I_0 + (QINSUR*DT)) > I_MAX ) then
       RUNSRF = QINSUR * DT - TOP_MAX_MOIST + TOP_MOIST
    else
       BASIS  = 1.0 - ( ( I_0 + (QINSUR * DT) ) / I_MAX )
       RUNSRF = QINSUR * DT - TOP_MAX_MOIST + TOP_MOIST + &
                TOP_MAX_MOIST * ( BASIS**(1.0+BVIC) )
    endif

    RUNSRF = RUNSRF / DT ! m/s
    if ( RUNSRF < 0.0 ) RUNSRF = 0.0
    if ( RUNSRF > QINSUR) RUNSRF = QINSUR

    PDDUM = QINSUR - RUNSRF    ! m/s

    end associate

  end subroutine RunoffSurfaceVIC

end module RunoffSurfaceVicMod
