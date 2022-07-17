module RunoffSurfaceBatsMod

!!! Calculate surface runoff based on TOPMODEL with groundwater scheme (Niu et al., 2007)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceBATS(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! local variables
    integer                :: K             ! loop index
    real(kind=kind_noahmp) :: SMCTOT        ! 2-m averaged soil moisture (m3/m3)
    real(kind=kind_noahmp) :: DZTOT         ! 2-m soil depth (m)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,   thickness of snow/soil layers (m)
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,   total soil water content [m3/m3]
              SoilImpervFrac             => noahmp%water%state%SoilImpervFrac              ,& ! in,   impervious fraction due to frozen soil
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow            ,& ! in,   water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,   saturated value of soil moisture [m3/m3]
              SoilSaturateFrac            => noahmp%water%state%SoilSaturateFrac             ,& ! out,  fractional saturated area for soil moisture
              RunoffSurface          => noahmp%water%flux%RunoffSurface            ,& ! out,  surface runoff [mm/s]
              InfilRateSfc           => noahmp%water%flux%InfilRateSfc              & ! out,  infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    SMCTOT = 0.0
    DZTOT  = 0.0

    ! compute mean soil moisture, depth and saturation fraction
    do K = 1, NumSoilLayer
       DZTOT   = DZTOT  + ThicknessSnowSoilLayer(K)
       SMCTOT  = SMCTOT + SoilMoisture(K) / SMCMAX(K) * ThicknessSnowSoilLayer(K)
       if ( DZTOT >= 2.0 ) exit
    enddo
    SMCTOT = SMCTOT / DZTOT
    SoilSaturateFrac   = max( 0.01, SMCTOT ) ** 4.0  ! BATS

    ! compute surface runoff and infiltration m/s
    if ( SoilSfcInflow > 0.0 ) then
       RunoffSurface = SoilSfcInflow * ( (1.0-SoilImpervFrac(1)) * SoilSaturateFrac + SoilImpervFrac(1) )
       InfilRateSfc  = SoilSfcInflow - RunoffSurface 
    endif

    end associate

  end subroutine RunoffSurfaceBATS

end module RunoffSurfaceBatsMod
