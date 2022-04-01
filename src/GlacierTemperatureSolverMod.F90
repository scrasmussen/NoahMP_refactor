module GlacierTemperatureSolverMod

!!! Compute Glacier and snow layer temperature using tri-diagonal matrix solution
!!! Dependent on the output from GlacierThermalDiffusion module

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use MatrixSolverTriDiagonalMod, only : MatrixSolverTriDiagonal

  implicit none

contains

  subroutine GlacierTemperatureSolver(noahmp, DT, AI, BI, CI, RHSTS)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: HSTEP_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: RHSTS  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: AI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: BI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: CI     ! left-hand side term of the matrix

! local variable
    integer                :: K         ! layer loop index 
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHSTSIN  ! temporary RHSTS matrix coefficient
    real(kind=kind_noahmp), allocatable, dimension(:) :: CIIN     ! temporary CI matrix coefficient

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of glacier/soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,    maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              STC             => noahmp%energy%state%STC              & ! inout, snow and glacier layer temperature [K]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTSIN (-NSNOW+1:NSOIL) )
    allocate( CIIN    (-NSNOW+1:NSOIL) )
    RHSTSIN  = 0.0
    CIIN     = 0.0

    ! update tri-diagonal matrix elements
    do K = ISNOW+1, NSOIL
       RHSTS(K) =    RHSTS(K) * DT
       AI(K)    =       AI(K) * DT
       BI(K)    = 1.0 + BI(K) * DT
       CI(K)    =       CI(K) * DT
    enddo

    ! copy values for input variables before call to rosr12
    do K = ISNOW+1, NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    enddo

    ! solve the tri-diagonal matrix equation
    call MatrixSolverTriDiagonal(CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)

    ! update snow & glacier temperature
    do K = ISNOW+1, NSOIL
       STC(K) = STC(K) + CI(K)
    enddo

    end associate

  end subroutine GlacierTemperatureSolver

end module GlacierTemperatureSolverMod
