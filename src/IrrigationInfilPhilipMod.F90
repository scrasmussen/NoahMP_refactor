module IrrigationInfilPhilipMod

!!! Estimate infiltration rate based on Philip's two parameter equation
!!! Reference: Eq.2 in Valiantzas (2010): New linearized two-parameter infiltration equation for direct
!!! determination of conductivity and sorptivity, J. Hydrology.

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine IrrigationInfilPhilip(noahmp, DT, FSUR)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: IRR_PHILIP_INFIL
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variables
    type(noahmp_type)     , intent(in)  :: noahmp
    real(kind=kind_noahmp), intent(in)  :: DT
    real(kind=kind_noahmp), intent(out) :: FSUR

! local variable
    integer                :: K         ! do loop/array indices
    integer                :: ISOIL     ! soil layer index
    real(kind=kind_noahmp) :: SP        ! sorptivity (LT^-1/2)
    real(kind=kind_noahmp) :: AP        ! intial hydraulic conductivity (m/s,L/T)
    real(kind=kind_noahmp) :: WCND      ! soil water conductivity [m/s]
    real(kind=kind_noahmp) :: WDF       ! soil water diffusivity (m2/s)
    real(kind=kind_noahmp) :: SICEMAX   ! maximum soil ice content (m3/m3)

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              DWSAT           => noahmp%water%param%DWSAT            ,& ! in,     saturated soil hydraulic diffusivity (m2/s)
              DKSAT           => noahmp%water%param%DKSAT             & ! in,     saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    ! initialize out-only and local variables
    WCND    = 0.0
    WDF     = 0.0
    SICEMAX = 0.0
    SP      = 0.0
    AP      = 0.0

    ! maximum ice fraction
    do K = 1, NSOIL
       if ( SICE(K) > SICEMAX ) SICEMAX = SICE(K)
    enddo

    ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
    ISOIL = 1
    call SoilDiffusivityConductivityOpt2(noahmp, WDF, WCND, SH2O(ISOIL), SICEMAX, ISOIL)

    ! sorptivity based on Eq. 10b from Kutilek, Miroslav, and Jana Valentova (1986) 
    ! sorptivity approximations. Transport in Porous Media 1.1, 57-62.
    SP = sqrt(2.0 * max(0.0, (SMCMAX(ISOIL) - SMC(ISOIL))) * (DWSAT(ISOIL) - WDF) )

    ! parameter A in Eq. 9 of Valiantzas (2010) is given by
    AP = min( WCND, (2.0/3.0) * DKSAT(ISOIL) )
    AP = max( AP  , (1.0/3.0) * DKSAT(ISOIL) )

    ! maximun infiltration rate, m/s
    FSUR = 0.5 * SP *( DT**(-0.5) ) + AP ! m/s
    FSUR = max( 0.0, FSUR )

    end associate

  end subroutine IrrigationInfilPhilip

end module IrrigationInfilPhilipMod
