module WaterTableEquilibriumMod

!!! Calculate equilibrium water table depth (Niu et al., 2005)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine WaterTableEquilibrium(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: ZWTEQ
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: K           ! do-loop index
    integer, parameter     :: NFINE = 100 ! no. of fine soil layers of 6m soil
    real(kind=kind_noahmp) :: WD1         ! water deficit from coarse (4-L) soil moisture profile
    real(kind=kind_noahmp) :: WD2         ! water deficit from fine (100-L) soil moisture profile
    real(kind=kind_noahmp) :: DZFINE      ! layer thickness of the 100-L soil layers to 6.0 m
    real(kind=kind_noahmp) :: TEMP        ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZFINE ! layer-bottom depth of the 100-L soil layers to 6.0 m
    allocate( ZFINE(1:NFINE) )

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              ZWT             => noahmp%water%state%ZWT               & ! out,    water table depth [m]
             )
! ----------------------------------------------------------------------

    ZFINE(1:NFINE) = 0.0
    WD1 = 0.0
    do K = 1, NSOIL
       WD1 = WD1 + (SMCMAX(1) - SH2O(K)) * DZSNSO(K) ! [m]
    enddo

    DZFINE = 3.0 * ( -ZSOIL(NSOIL) ) / NFINE
    do K = 1, NFINE
       ZFINE(K) = float(K) * DZFINE
    enddo

    ZWT = -3.0 * ZSOIL(NSOIL) - 0.001   ! initial value [m]

    WD2 = 0.0
    do K = 1, NFINE
       TEMP  = 1.0 + ( ZWT - ZFINE(K) ) / PSISAT(1)
       WD2   = WD2 + SMCMAX(1) * ( 1.0 - TEMP ** (-1.0/BEXP(1)) ) * DZFINE
       if ( abs(WD2-WD1) <= 0.01 ) then
          ZWT = ZFINE(K)
          exit
       endif
    enddo

    end associate

  end subroutine WaterTableEquilibrium

end module WaterTableEquilibriumMod
