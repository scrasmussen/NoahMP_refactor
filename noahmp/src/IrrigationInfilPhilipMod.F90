module IrrigationInfilPhilipMod

!!! Estimate infiltration rate based on Philip's two parameter equation
!!! Reference: Eq.2 in Valiantzas (2010): New linearized two-parameter infiltration equation for direct
!!! determination of conductivity and sorptivity, J. Hydrology.

  use Machine
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
    type(noahmp_type)     , intent(inout)  :: noahmp
    real(kind=kind_noahmp), intent(in)     :: DT
    real(kind=kind_noahmp), intent(out)    :: FSUR

! local variable
    integer                :: K         ! do loop/array indices
    integer                :: ISOIL     ! soil layer index
    real(kind=kind_noahmp) :: SP        ! sorptivity (LT^-1/2)
    real(kind=kind_noahmp) :: AP        ! intial hydraulic conductivity (m/s,L/T)
    real(kind=kind_noahmp) :: SoilWatConductivity      ! soil water conductivity [m/s]
    real(kind=kind_noahmp) :: SoilWatDiffusivity       ! soil water diffusivity (m2/s)
    real(kind=kind_noahmp) :: SoilIceMaxTmp   ! maximum soil ice content (m3/m3)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,  number of soil layers
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,  total soil moisture [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,  soil water content [m3/m3]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,  soil ice content [m3/m3]
              SoilMoistureSat          => noahmp%water%param%SoilMoistureSat           ,& ! in,  saturated value of soil moisture [m3/m3]
              SoilWatDiffusivitySat           => noahmp%water%param%SoilWatDiffusivitySat            ,& ! in,  saturated soil hydraulic diffusivity [m2/s]
              SoilWatConductivitySat           => noahmp%water%param%SoilWatConductivitySat             & ! in,  saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    ! initialize out-only and local variables
    SoilWatConductivity    = 0.0
    SoilWatDiffusivity     = 0.0
    SoilIceMaxTmp = 0.0
    SP      = 0.0
    AP      = 0.0

    ! maximum ice fraction
    do K = 1, NumSoilLayer
       if ( SoilIce(K) > SoilIceMaxTmp ) SoilIceMaxTmp = SoilIce(K)
    enddo

    ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
    ISOIL = 1
    call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                         SoilLiqWater(ISOIL), SoilIceMaxTmp, ISOIL)

    ! sorptivity based on Eq. 10b from Kutilek, Miroslav, and Jana Valentova (1986) 
    ! sorptivity approximations. Transport in Porous Media 1.1, 57-62.
    SP = sqrt(2.0 * max(0.0, (SoilMoistureSat(ISOIL) - SoilMoisture(ISOIL))) * &
              (SoilWatDiffusivitySat(ISOIL) - SoilWatDiffusivity) )

    ! parameter A in Eq. 9 of Valiantzas (2010) is given by
    AP = min( SoilWatConductivity, (2.0/3.0) * SoilWatConductivitySat(ISOIL) )
    AP = max( AP  , (1.0/3.0) * SoilWatConductivitySat(ISOIL) )

    ! maximun infiltration rate, m/s
    FSUR = 0.5 * SP *( DT**(-0.5) ) + AP ! m/s
    FSUR = max( 0.0, FSUR )

    end associate

  end subroutine IrrigationInfilPhilip

end module IrrigationInfilPhilipMod
