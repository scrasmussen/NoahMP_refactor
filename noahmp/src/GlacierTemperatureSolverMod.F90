module GlacierTemperatureSolverMod

!!! Compute Glacier and snow layer temperature using tri-diagonal matrix solution
!!! Dependent on the output from GlacierThermalDiffusion module

  use Machine
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
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,& ! in,    number of glacier/soil layers
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,& ! in,    maximum number of snow layers
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg ,& ! in,    actual number of snow layers (negative)
              STC             => noahmp%energy%state%STC              & ! inout, snow and glacier layer temperature [K]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTSIN (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( CIIN    (-NumSnowLayerMax+1:NumSoilLayer) )
    RHSTSIN  = 0.0
    CIIN     = 0.0

    ! update tri-diagonal matrix elements
    do K = NumSnowLayerNeg+1, NumSoilLayer
       RHSTS(K) =    RHSTS(K) * DT
       AI(K)    =       AI(K) * DT
       BI(K)    = 1.0 + BI(K) * DT
       CI(K)    =       CI(K) * DT
    enddo

    ! copy values for input variables before call to rosr12
    do K = NumSnowLayerNeg+1, NumSoilLayer
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    enddo

    ! solve the tri-diagonal matrix equation
    call MatrixSolverTriDiagonal(CI,AI,BI,CIIN,RHSTSIN,RHSTS,NumSnowLayerNeg+1,NumSoilLayer,NumSnowLayerMax)

    ! update snow & glacier temperature
    do K = NumSnowLayerNeg+1, NumSoilLayer
       STC(K) = STC(K) + CI(K)
    enddo

    end associate

  end subroutine GlacierTemperatureSolver

end module GlacierTemperatureSolverMod
