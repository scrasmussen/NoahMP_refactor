module SoilWaterInfilGreenAmptMod

!!! Compute  soil surface infiltration rate based on Green-Ampt equation
!!! We use its three parameter version of the smith-parlage equation, where gamma = 0, Eq 6.25 = Green-Ampt.
!!! Reference: Smith, R.E. (2002) Infiltration Theory for Hydrologic Applications, Water Resources Monograph

  use Machine
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
    integer               , intent(in)    :: INFLMAX  ! check for maximum infiltration at SoilMoistureWilt 
    real(kind=kind_noahmp), intent(inout) :: FACC     ! accumulated infiltration rate (m/s)
    real(kind=kind_noahmp), intent(out)   :: FSUR     ! surface infiltration rate (m/s)

! local variable
    integer                :: ISOIL      ! soil layer index
    real(kind=kind_noahmp) :: SoilWatDiffusivity        ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp) :: SoilWatConductivity       ! soil water conductivity[m/s]
    real(kind=kind_noahmp) :: JJ         ! dummy variable

! --------------------------------------------------------------------
    associate(                                                        &
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,     depth [m] of layer-bottom from soil surface
              SoilMoisture             => noahmp%water%state%SoilMoisture             ,& ! in,     total soil moisture [m3/m3]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,     soil ice content [m3/m3] 
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow            ,& ! in,     water input on soil surface [mm/s]
              SoilMoistureSat          => noahmp%water%param%SoilMoistureSat           ,& ! in,     saturated value of soil moisture [m3/m3]
              SoilMoistureWilt          => noahmp%water%param%SoilMoistureWilt           ,& ! in,     wilting point soil moisture [m3/m3]
              SoilWatConductivitySat           => noahmp%water%param%SoilWatConductivitySat            ,& ! in,     saturated soil hydraulic conductivity [m/s]
              InfilCapillaryDynVic           => noahmp%water%param%InfilCapillaryDynVic             & ! in,     DVIC Mean Capillary Drive (m) for infiltration models
              )
! ----------------------------------------------------------------------

    ISOIL = 1
    if ( INFLMAX == 1 ) then

       ! estimate initial soil hydraulic conductivty (Ki in the equation) (m/s)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoistureWilt(ISOIL), 0.0, ISOIL)

       ! Maximum infiltrability based on the Eq. 6.25. (m/s)
       JJ   = InfilCapillaryDynVic * (SoilMoistureSat(ISOIL) - SoilMoistureWilt(ISOIL)) * (-1.0) * DepthSoilLayer(ISOIL)
       FSUR = SoilWatConductivitySat(ISOIL) + ( (JJ/1.0e-05) * (SoilWatConductivitySat(ISOIL) - SoilWatConductivity) )

       !maximum infiltration rate at surface
       if ( FSUR < 0.0 ) FSUR = SoilWatConductivity

    else

       ! estimate initial soil hydraulic conductivty (Ki in the equation) (m/s)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoisture(ISOIL), SoilIce(ISOIL), ISOIL)

       ! Maximum infiltrability based on the Eq. 6.25. (m/s)
       JJ   = InfilCapillaryDynVic * max(0.0, (SoilMoistureSat(ISOIL) - SoilMoisture(ISOIL))) * (-1.0) * DepthSoilLayer(ISOIL)
       FSUR = SoilWatConductivitySat(ISOIL) + ( (JJ/FACC) * (SoilWatConductivitySat(ISOIL) - SoilWatConductivity) )

       ! infiltration rate at surface
       if ( SoilWatConductivitySat(ISOIL) < SoilSfcInflow ) then
          FSUR = min( SoilSfcInflow, FSUR )
       else
          FSUR = SoilSfcInflow
       endif
       ! accumulated infiltration function
       FACC = FACC + FSUR

    endif

    end associate

  end subroutine SoilWaterInfilGreenAmpt

end module SoilWaterInfilGreenAmptMod
