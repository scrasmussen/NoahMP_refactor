module RunoffSurfaceTopModelGrdMod

!!! Calculate surface runoff based on TOPMODEL with groundwater scheme (Niu et al., 2007)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceTopModelGrd(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow            ,& ! in,     water input on soil surface [mm/s]
              RunoffDecayFac             => noahmp%water%param%RunoffDecayFac              ,& ! in,     runoff decay factor (m-1)
              SoilSfcSatFracMax          => noahmp%water%param%SoilSfcSatFracMax           ,& ! in,     maximum surface saturated fraction (global mean)
              SoilImpervFrac             => noahmp%water%state%SoilImpervFrac              ,& ! in,     impervious fraction due to frozen soil
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! in,     water table depth [m]
              SoilSaturateFrac            => noahmp%water%state%SoilSaturateFrac             ,& ! out,    fractional saturated area for soil moisture
              RunoffSurface          => noahmp%water%flux%RunoffSurface            ,& ! out,    surface runoff [mm/s]
              InfilRateSfc           => noahmp%water%flux%InfilRateSfc              & ! out,    infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! set up key parameter
    RunoffDecayFac = 6.0

    ! compute saturated area fraction
    SoilSaturateFrac = SoilSfcSatFracMax * exp( -0.5 * RunoffDecayFac * (WaterTableDepth-2.0) )

    ! compute surface runoff and infiltration  m/s
    if ( SoilSfcInflow > 0.0 ) then
       RunoffSurface = SoilSfcInflow * ( (1.0-SoilImpervFrac(1)) * SoilSaturateFrac + SoilImpervFrac(1) )
       InfilRateSfc  = SoilSfcInflow - RunoffSurface 
    endif

    end associate

  end subroutine RunoffSurfaceTopModelGrd

end module RunoffSurfaceTopModelGrdMod
