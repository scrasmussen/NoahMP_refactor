module WaterTableEquilibriumMod

!!! Calculate equilibrium water table depth (Niu et al., 2005)

  use Machine
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
    real(kind=kind_noahmp), dimension(1:NFINE) :: ZFINE ! layer-bottom depth of the 100-L soil layers to 6.0 m

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              DepthSoilLayer  => noahmp%config%domain%DepthSoilLayer ,& ! in,   depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,   thickness of snow/soil layers (m)
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,   soil water content [m3/m3]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,   saturated value of soil moisture [m3/m3]
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,   saturated soil matric potential (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,   soil B parameter
              WaterTableDepth             => noahmp%water%state%WaterTableDepth               & ! out,  water table depth [m]
             )
! ----------------------------------------------------------------------

    ZFINE(1:NFINE) = 0.0
    WD1 = 0.0
    do K = 1, NumSoilLayer
       WD1 = WD1 + (SMCMAX(1) - SoilLiqWater(K)) * ThicknessSnowSoilLayer(K) ! [m]
    enddo

    DZFINE = 3.0 * ( -DepthSoilLayer(NumSoilLayer) ) / NFINE
    do K = 1, NFINE
       ZFINE(K) = float(K) * DZFINE
    enddo

    WaterTableDepth = -3.0 * DepthSoilLayer(NumSoilLayer) - 0.001   ! initial value [m]

    WD2 = 0.0
    do K = 1, NFINE
       TEMP  = 1.0 + ( WaterTableDepth - ZFINE(K) ) / PSISAT(1)
       WD2   = WD2 + SMCMAX(1) * ( 1.0 - TEMP ** (-1.0/BEXP(1)) ) * DZFINE
       if ( abs(WD2-WD1) <= 0.01 ) then
          WaterTableDepth = ZFINE(K)
          exit
       endif
    enddo

    end associate

  end subroutine WaterTableEquilibrium

end module WaterTableEquilibriumMod
