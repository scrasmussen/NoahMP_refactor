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
              SNOW_EMIS       => noahmp%energy%param%SNOW_EMIS       ,& ! in,    snow emissivity
              EG              => noahmp%energy%param%EG              ,& ! in,    emissivity soil surface
              EICE            => noahmp%energy%param%EICE            ,& ! in,    emissivity ice surface
              FSNO            => noahmp%water%state%FSNO             ,& ! in,    snow cover fraction (-)
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
       EMG = EICE * (1.0 - FSNO) + SNOW_EMIS * FSNO  ! move hard-coded snow emissivity as a global parameter to MPTABLE
    else
       EMG = EG(SurfaceType) * (1.0 - FSNO) + SNOW_EMIS * FSNO
    endif

    ! net surface emissivity
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + (1-FVEG) * EMG

    end associate

  end subroutine SurfaceEmissivity

end module SurfaceEmissivityMod
