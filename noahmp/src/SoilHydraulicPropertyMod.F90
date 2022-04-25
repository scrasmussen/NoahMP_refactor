module SoilHydraulicPropertyMod

!!! Two methods for calculating soil water diffusivity and soil hydraulic conductivity
!!! Option 1: linear effects (more permeable, Niu and Yang,2006); Option 2: nonlinear effects (less permeable)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilDiffusivityConductivityOpt1(noahmp, WDF, WCND, SMC, FCR, ISOIL)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: WDFCND1
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN and OUT variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: ISOIL    ! soil layer index
    real(kind=kind_noahmp), intent(in)    :: SMC      ! soil moisture [m3/m3]
    real(kind=kind_noahmp), intent(in)    :: FCR      ! impermeable fraction due to frozen soil
    real(kind=kind_noahmp), intent(out)   :: WCND     ! soil water conductivity [m/s]
    real(kind=kind_noahmp), intent(out)   :: WDF      ! soil water diffusivity (m2/s)

! local variable
    real(kind=kind_noahmp)                :: EXPON    ! exponential local factor
    real(kind=kind_noahmp)                :: FACTR    ! pre-factor

! --------------------------------------------------------------------
    associate(                                                        &
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              DWSAT           => noahmp%water%param%DWSAT            ,& ! in,     saturated soil hydraulic diffusivity (m2/s)
              DKSAT           => noahmp%water%param%DKSAT             & ! in,     saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    FACTR = max( 0.01, SMC/SMCMAX(ISOIL) )

    ! soil water diffusivity
    EXPON = BEXP(ISOIL) + 2.0
    WDF   = DWSAT(ISOIL) * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

    ! soil hydraulic conductivity
    EXPON = 2.0 * BEXP(ISOIL) + 3.0
    WCND  = DKSAT(ISOIL) * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

    end associate

  end subroutine SoilDiffusivityConductivityOpt1


  subroutine SoilDiffusivityConductivityOpt2(noahmp, WDF, WCND, SMC, SICE, ISOIL)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: WDFCND2
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN and OUT variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: ISOIL    ! soil layer index
    real(kind=kind_noahmp), intent(in)    :: SMC      ! soil moisture [m3/m3]
    real(kind=kind_noahmp), intent(in)    :: SICE     ! soil ice content [m3/m3]
    real(kind=kind_noahmp), intent(out)   :: WCND     ! soil water conductivity [m/s]
    real(kind=kind_noahmp), intent(out)   :: WDF      ! soil water diffusivity (m2/s)

! local variable
    real(kind=kind_noahmp)                :: EXPON    ! exponential local factor
    real(kind=kind_noahmp)                :: FACTR1   ! pre-factor
    real(kind=kind_noahmp)                :: FACTR2   ! pre-factor
    real(kind=kind_noahmp)                :: VKWGT    ! weights

! --------------------------------------------------------------------
    associate(                                                        &
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              BEXP            => noahmp%water%param%BEXP             ,& ! in,     soil B parameter
              DWSAT           => noahmp%water%param%DWSAT            ,& ! in,     saturated soil hydraulic diffusivity (m2/s)
              DKSAT           => noahmp%water%param%DKSAT             & ! in,     saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    FACTR1 = 0.05 / SMCMAX(ISOIL)
    FACTR2 = max( 0.01, SMC/SMCMAX(ISOIL) )
    FACTR1 = min( FACTR1, FACTR2 )

    ! soil water diffusivity
    EXPON  = BEXP(ISOIL) + 2.0
    WDF    = DWSAT(ISOIL) * FACTR2 ** EXPON
    if ( SICE > 0.0 ) then
       VKWGT = 1.0 / ( 1.0 + (500.0 * SICE)**3.0 )
       WDF   = VKWGT * WDF + (1.0-VKWGT) * DWSAT(ISOIL) * FACTR1 ** EXPON
    endif

    ! soil hydraulic conductivity
    EXPON = 2.0 * BEXP(ISOIL) + 3.0
    WCND  = DKSAT(ISOIL) * FACTR2 ** EXPON

    end associate

  end subroutine SoilDiffusivityConductivityOpt2

end module SoilHydraulicPropertyMod
