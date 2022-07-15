module SoilWaterInfilPhilipMod

!!! Compute soil surface infiltration rate based on Philip's two parameter equation
!!! Reference: Valiantzas (2010): New linearized two-parameter infiltration equation 
!!! for direct determination of conductivity and sorptivity, J. Hydrology.

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine SoilWaterInfilPhilip(noahmp, DT, INFLMAX, FACC, FSUR)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: PHILIP_INFIL
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: INFLMAX  ! check for maximum infiltration at SMCWLT 
    real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), intent(inout) :: FACC     ! accumulated infiltration rate (m/s)
    real(kind=kind_noahmp), intent(out)   :: FSUR     ! surface infiltration rate (m/s)

! local variable
    integer                :: ISOIL      ! soil layer index
    real(kind=kind_noahmp) :: SoilWatDiffusivity        ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp) :: SoilWatConductivity       ! soil water conductivity[m/s]
    real(kind=kind_noahmp) :: SP         ! sorptivity (LT^-1/2)
    real(kind=kind_noahmp) :: AP         ! intial hydraulic conductivity (m/s,L/T)
    real(kind=kind_noahmp) :: FMAX       ! Maximum infiltration (m/s)

! --------------------------------------------------------------------
    associate(                                                        &
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,     total soil moisture [m3/m3]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,     soil ice content [m3/m3] 
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,     wilting point soil moisture [m3/m3]
              DWSAT           => noahmp%water%param%DWSAT            ,& ! in,     saturated soil hydraulic diffusivity (m2/s)
              DKSAT           => noahmp%water%param%DKSAT             & ! in,     saturated soil hydraulic conductivity [m/s]
              )
! ----------------------------------------------------------------------

    ISOIL = 1
    if ( INFLMAX == 1) then

       ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, SMCWLT(ISOIL), 0.0, ISOIL)

       ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986)
       ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
       SP = sqrt(2.0 * (SMCMAX(ISOIL) - SMCWLT(ISOIL)) * (DWSAT(ISOIL) - SoilWatDiffusivity))

       ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
       AP = min( SoilWatConductivity, (2.0/3.0)*DKSAT(ISOIL) )
       AP = max( AP,   (1.0/3.0)*DKSAT(ISOIL) )

       ! Maximun infiltration rate, m
       FSUR = (1.0/2.0) * SP * (DT**(-1.0/2.0)) + AP ! m/s
       if ( FSUR < 0.0) FSUR = SoilWatConductivity

    else

       ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoisture(ISOIL), SoilIce(ISOIL), ISOIL)

       ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986) 
       ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
       SP = sqrt( 2.0 * max(0.0, (SMCMAX(ISOIL)-SoilMoisture(ISOIL))) * (DWSAT(ISOIL) - SoilWatDiffusivity) )
       ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
       AP = min( SoilWatConductivity, (2.0/3.0)*DKSAT(ISOIL) )
       AP = max( AP,   (1.0/3.0)*DKSAT(ISOIL) )

       ! Maximun infiltration rate, m
       FSUR = (1.0/2.0) * SP * (DT**(-1.0/2.0)) + AP ! m/s

       ! infiltration rate at surface
       if ( DKSAT(ISOIL) < QINSUR ) then
          FSUR = min( QINSUR, FSUR )
       else
          FSUR = QINSUR
       endif
       ! accumulated infiltration function
       FACC = FACC + FSUR

    endif

    end associate

  end subroutine SoilWaterInfilPhilip

end module SoilWaterInfilPhilipMod
