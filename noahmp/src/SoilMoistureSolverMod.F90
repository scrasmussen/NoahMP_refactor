module SoilMoistureSolverMod

!!! Compute soil moisture content using based on Richards diffusion & tri-diagonal matrix
!!! Dependent on the output from SoilWaterDiffusionRichards subroutine

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use MatrixSolverTriDiagonalMod, only : MatrixSolverTriDiagonal

  implicit none

contains

  subroutine SoilMoistureSolver(noahmp, DT, AI, BI, CI, RHSTT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SSTEP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: RHSTT  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: AI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: BI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: CI     ! left-hand side term of the matrix

! local variable
    integer                :: K         ! soil layer loop index 
    real(kind=kind_noahmp) :: WMINUS    ! temporary water deficiency
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHSTTIN  ! temporary RHSTT matrix coefficient
    real(kind=kind_noahmp), allocatable, dimension(:) :: CIIN     ! temporary CI matrix coefficient

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              OptRunoffSubsurface => noahmp%config%nmlist%OptRunoffSubsurface ,& ! in,     options for drainage and subsurface runoff
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              ZWT             => noahmp%water%state%ZWT              ,& ! in,     water table depth [m]
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              SMCWTD          => noahmp%water%state%SMCWTD           ,& ! inout,  soil moisture between bottom of the soil and the water table
              DEEPRECH        => noahmp%water%state%DEEPRECH         ,& ! inout,  recharge to or from the water table when deep [m]
              QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! inout,  soil bottom drainage (m/s)
              EPORE           => noahmp%water%state%EPORE_SOIL       ,& ! out,    soil effective porosity (m3/m3)
              WPLUS           => noahmp%water%state%WPLUS             & ! out,    saturation excess of the total soil [m]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTTIN(1:NSOIL) )
    allocate( CIIN   (1:NSOIL) )
    RHSTTIN  = 0.0
    CIIN     = 0.0
    WPLUS    = 0.0
    EPORE(:) = 0.0

    ! update tri-diagonal matrix elements
    do K = 1, NSOIL
       RHSTT (K) =    RHSTT(K) * DT
       AI (K)    =       AI(K) * DT
       BI (K)    = 1.0 + BI(K) * DT
       CI (K)    =       CI(K) * DT
    enddo

    ! copy values for input variables before calling rosr12
    do K = 1, NSOIL
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    enddo

    ! call ROSR12 to solve the tri-diagonal matrix
    call MatrixSolverTriDiagonal(CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NSOIL,0)

    do K = 1, NSOIL
        SH2O(K) = SH2O(K) + CI(K)
    enddo

    !  excessive water above saturation in a layer is moved to
    !  its unsaturated layer like in a bucket

    ! for MMF scheme, there is soil moisture below nsoil, to the water table
    if ( OptRunoffSubsurface == 5 ) then
       ! update smcwtd
       if ( ZWT < ZSOIL(NSOIL)-DZSNSO(NSOIL) ) then
          ! accumulate qdrain to update deep water table and soil moisture later
          DEEPRECH =  DEEPRECH + DT * QDRAIN
       else
          SMCWTD      = SMCWTD + DT * QDRAIN  / DZSNSO(NSOIL)
          WPLUS       = max( (SMCWTD - SMCMAX(NSOIL)), 0.0 ) * DZSNSO(NSOIL)
          WMINUS      = max( (1.0e-4 - SMCWTD), 0.0 ) * DZSNSO(NSOIL)
          SMCWTD      = max( min(SMCWTD, SMCMAX(NSOIL)), 1.0e-4 )
          SH2O(NSOIL) = SH2O(NSOIL) + WPLUS / DZSNSO(NSOIL)
          ! reduce fluxes at the bottom boundaries accordingly
          QDRAIN   = QDRAIN - WPLUS/DT
          DEEPRECH = DEEPRECH - WMINUS
       endif
    endif

    do K = NSOIL, 2, -1
       EPORE(K)  = max( 1.0e-4, (SMCMAX(K) - SICE(K)) )
       WPLUS     = max( (SH2O(K)-EPORE(K)), 0.0 ) * DZSNSO(K)
       SH2O(K)   = min( EPORE(K), SH2O(K) )
       SH2O(K-1) = SH2O(K-1) + WPLUS / DZSNSO(K-1)
    enddo

    EPORE(1) = max( 1.0e-4, (SMCMAX(1) - SICE(1)) )
    WPLUS    = max( (SH2O(1)-EPORE(1)), 0.0 ) * DZSNSO(1)
    SH2O(1)  = min( EPORE(1), SH2O(1) )

    if ( WPLUS > 0.0 ) then
       SH2O(2) = SH2O(2) + WPLUS / DZSNSO(2)
       do K = 2, NSOIL-1
          EPORE(K)  = max( 1.0e-4, (SMCMAX(K) - SICE(K)) )
          WPLUS     = max( (SH2O(K)-EPORE(K)), 0.0 ) * DZSNSO(K)
          SH2O(K)   = min( EPORE(K), SH2O(K) )
          SH2O(K+1) = SH2O(K+1) + WPLUS / DZSNSO(K+1)
       enddo
       EPORE(NSOIL) = max( 1.0e-4, (SMCMAX(NSOIL) - SICE(NSOIL)) )
       WPLUS        = max( (SH2O(NSOIL)-EPORE(NSOIL)), 0.0 ) * DZSNSO(NSOIL)
       SH2O(NSOIL)  = min( EPORE(NSOIL), SH2O(NSOIL) )
    endif

    SMC = SH2O + SICE

    end associate

  end subroutine SoilMoistureSolver

end module SoilMoistureSolverMod
