module SurfaceEmissivityMod

!!! Compute ground, vegetation, and total surface longwave emissivity

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceEmissivity(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              IndicatorIceSfc => noahmp%config%domain%IndicatorIceSfc ,& ! in,    indicator for ice point: 1->seaice; -1->land ice; 0->soil
              SurfaceType     => noahmp%config%domain%SurfaceType     ,& ! in,    surface type 1-soil; 2-lake
              EmissivitySnow       => noahmp%energy%param%EmissivitySnow       ,& ! in,    snow emissivity
              EmissivitySoilLake              => noahmp%energy%param%EmissivitySoilLake              ,& ! in,    emissivity soil surface
              EmissivityIceSfc            => noahmp%energy%param%EmissivityIceSfc            ,& ! in,    emissivity ice surface
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,    snow cover fraction [-]
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              EMV             => noahmp%energy%state%EMV             ,& ! out,   vegetation emissivity
              EMG             => noahmp%energy%state%EMG             ,& ! out,   ground emissivity
              EMISSI          => noahmp%energy%state%EMISSI           & ! out,   surface emissivity
             )
! ----------------------------------------------------------------------

    ! vegetation emissivity
    EMV = 1.0 - exp( -(ELAI + ESAI) / 1.0 )

    ! ground emissivity
    if ( IndicatorIceSfc == 1 ) then
       EMG = EmissivityIceSfc * (1.0 - SnowCoverFrac) + EmissivitySnow * SnowCoverFrac
    else
       EMG = EmissivitySoilLake(SurfaceType) * (1.0 - SnowCoverFrac) + EmissivitySnow * SnowCoverFrac
    endif

    ! net surface emissivity
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + (1-FVEG) * EMG

    end associate

  end subroutine SurfaceEmissivity

end module SurfaceEmissivityMod
