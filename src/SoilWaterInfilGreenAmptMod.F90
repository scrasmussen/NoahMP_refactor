module SoilWaterInfilGreenAmptMod

!!! Compute  soil surface infiltration rate based on Green-Ampt equation
!!! We use its three parameter version of the smith-parlage equation, where gamma = 0, Eq 6.25 = Green-Ampt.
!!! Reference: Smith, R.E. (2002) Infiltration Theory for Hydrologic Applications, Water Resources Monograph

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine SoilWaterInfilGreenAmpt(noahmp, INFLMAX, FACC, FSUR)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: GREEN_AMPT_INFIL
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: INFLMAX  ! check for maximum infiltration at SMCWLT 
    real(kind=kind_noahmp), intent(inout) :: FACC     ! accumulated infiltration rate (m/s)
    real(kind=kind_noahmp), intent(out)   :: FSUR     ! surface infiltration rate (m/s)

! local variable
    integer                :: ISOIL      ! soil layer index
    real(kind=kind_noahmp) :: WDF        ! soil water diffusivity (m2/s)
    real(kind=kind_noahmp) :: WCND       ! soil water conductivity[m/s]
    real(kind=kind_noahmp) :: JJ         ! dummy variable

! --------------------------------------------------------------------
    associate(                                                        &
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3] 
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,     wilting point soil moisture [m3/m3]
              DKSAT           => noahmp%water%param%DKSAT            ,& ! in,     saturated soil hydraulic conductivity [m/s]
              GDVIC           => noahmp%water%param%GDVIC             & ! in,     DVIC Mean Capillary Drive (m) for infiltration models
              )
! ----------------------------------------------------------------------

    ISOIL = 1
    if ( INFLMAX == 1 ) then

       ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
       call SoilDiffusivityConductivityOpt2(noahmp, WDF, WCND, SMCWLT(ISOIL), 0.0, ISOIL)

       ! Maximum infiltrability based on the Eq. 6.25. (m/s)
       JJ   = GDVIC * (SMCMAX(ISOIL) - SMCWLT(ISOIL)) * (-1.0) * ZSOIL(ISOIL)
       FSUR = DKSAT(ISOIL) + ( (JJ/1.0e-05) * (DKSAT(ISOIL) - WCND) )

       !maximum infiltration rate at surface
       if ( FSUR < 0.0 ) FSUR = WCND

    else

       ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
       call SoilDiffusivityConductivityOpt2(noahmp, WDF, WCND, SMC(ISOIL), SICE(ISOIL), ISOIL)

       ! Maximum infiltrability based on the Eq. 6.25. (m/s)
       JJ   = GDVIC * max(0.0, (SMCMAX(ISOIL) - SMC(ISOIL))) * (-1.0) * ZSOIL(ISOIL)
       FSUR = DKSAT(ISOIL) + ( (JJ/FACC) * (DKSAT(ISOIL) - WCND) )

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

  end subroutine SoilWaterInfilGreenAmpt

end module SoilWaterInfilGreenAmptMod
