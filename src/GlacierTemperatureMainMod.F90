module GlacierTemperatureMainMod

!!! Main module to compute snow (if exists) and glacier ice temperature. 
!!! Note that snow temperatures during melting season may exceed melting 
!!! point (TFRZ) but later in GlacierPhaseChange subroutine the snow
!!! temperatures are reset to TFRZ for melting snow.

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use GlacierTemperatureSolverMod, only : GlacierTemperatureSolver
  use GlacierThermalDiffusionMod,  only : GlacierThermalDiffusion 

  implicit none

contains

  subroutine GlacierTemperatureMain(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TSNOSOI_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                :: IZ         ! loop index
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHSTS  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: AI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: BI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: CI     ! left-hand side term

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of glacier/soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,    maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              ZBOT            => noahmp%energy%param%ZBOT            ,& ! in,    depth of lower boundary condition (m) from glacier/soil surface
              ZBOTSNO         => noahmp%energy%state%ZBOTSNO         ,& ! out,   depth of lower boundary condition (m) from snow surface
              PHI             => noahmp%energy%flux%PHI               & ! out,   light penetrating through snow/ice (W/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTS (-NSNOW+1:NSOIL) )
    allocate( AI    (-NSNOW+1:NSOIL) )
    allocate( BI    (-NSNOW+1:NSOIL) )
    allocate( CI    (-NSNOW+1:NSOIL) )
    RHSTS(:) = 0.0
    AI(:)    = 0.0
    BI(:)    = 0.0
    CI(:)    = 0.0

    ! compute solar penetration through water, needs more work
    PHI(ISNOW+1:NSOIL) = 0.0

    ! adjust ZBOT from glacier ice surface to ZBOTSNO from snow surface
    ZBOTSNO = ZBOT - SNOWH

    ! compute soil temperatures
    call GlacierThermalDiffusion(noahmp, AI, BI, CI, RHSTS)
    call GlacierTemperatureSolver(noahmp, DT, AI, BI, CI, RHSTS)

    end associate

  end subroutine GlacierTemperatureMain

end module GlacierTemperatureMainMod
