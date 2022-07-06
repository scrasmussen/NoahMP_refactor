module PhenologyMainMod

!!! Main Phenology module to estimate vegetation phenology (LAI, SAI, FVEG)
!!! considering vegeation canopy being buries by snow and evolution in time

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PhenologyMain (noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PHENOLOGY
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variables
    integer                          :: K           ! index
    integer                          :: IT1,IT2     ! interpolation months
    real(kind=kind_noahmp)           :: DB          ! thickness of canopy buried by snow (m)
    real(kind=kind_noahmp)           :: FB          ! fraction of canopy buried by snow
    real(kind=kind_noahmp)           :: SNOWHC      ! critical snow depth at which short vege is fully covered by snow
    real(kind=kind_noahmp)           :: DAY         ! current day of year (0<=DAY<NumDayInYear)
    real(kind=kind_noahmp)           :: WT1,WT2     ! interpolation weights
    real(kind=kind_noahmp)           :: T           ! current month (1.00, ..., 12.00)

!---------------------------------------------------------------------
    associate(                                                     &
              OptDynamicVeg   => noahmp%config%nmlist%OptDynamicVeg  ,& ! in,    dynamic vegetation option
              OptCropModel    => noahmp%config%nmlist%OptCropModel   ,& ! in,    crop model option
              VegType         => noahmp%config%domain%VegType        ,& ! in,    vegetation type 
              CropType        => noahmp%config%domain%CropType       ,& ! in,    crop type 
              IndexIcePoint           => noahmp%config%domain%IndexIcePoint       ,& ! in,    land ice flag
              IndexBarrenPoint        => noahmp%config%domain%IndexBarrenPoint    ,& ! in,    bare soil flag
              IndexWaterPoint         => noahmp%config%domain%IndexWaterPoint     ,& ! in,    water point flag
              FlagUrban      => noahmp%config%domain%FlagUrban  ,& ! in,    urban point flag
              FlagDynamicVeg     => noahmp%config%domain%FlagDynamicVeg ,& ! in,    flag to activate dynamic vegetation model
              FlagDynamicCrop     => noahmp%config%domain%FlagDynamicCrop ,& ! in,    flag to activate dynamic crop model
              Latitude        => noahmp%config%domain%Latitude    ,& ! in,    latitude (degree)
              NumDayInYear    => noahmp%config%domain%NumDayInYear ,& ! in,    Number of days in the particular year
              DayJulianInYear          => noahmp%config%domain%DayJulianInYear      ,& ! in,    Julian day of year
              HVT             => noahmp%energy%param%HVT          ,& ! in,    top of canopy (m)
              HVB             => noahmp%energy%param%HVB          ,& ! in,    bottom of canopy (m)
              LAIM            => noahmp%energy%param%LAIM         ,& ! in,    monthly leaf area index, one-sided
              SAIM            => noahmp%energy%param%SAIM         ,& ! in,    monthly stem area index, one-sided
              SHDMAX          => noahmp%energy%param%SHDMAX       ,& ! in,    yearly maximum vegetation fraction
              SHDFAC          => noahmp%energy%param%SHDFAC       ,& ! in,    green vegetation fraction
              TMIN            => noahmp%biochem%param%TMIN        ,& ! in,    minimum temperature for photosynthesis (k)
              PlantGrowStage             => noahmp%biochem%state%PlantGrowStage         ,& ! in,    plant growing stage
              SNOWH           => noahmp%water%state%SNOWH         ,& ! in,    snow height [m]
              TV              => noahmp%energy%state%TV           ,& ! in,    vegetation temperature (k)
              TROOT           => noahmp%energy%state%TROOT        ,& ! in,    root-zone averaged temperature (k)
              LAI             => noahmp%energy%state%LAI          ,& ! inout, LAI, unadjusted for burying by snow
              SAI             => noahmp%energy%state%SAI          ,& ! inout, SAI, unadjusted for burying by snow
              ELAI            => noahmp%energy%state%ELAI         ,& ! out,   leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI         ,& ! out,   stem area index, after burying by snow
              FVEG            => noahmp%energy%state%FVEG         ,& ! out,   green vegetation fraction 
              IndexGrowSeason             => noahmp%biochem%state%IndexGrowSeason          & ! out,   growing season index (0=off, 1=on)
             )                    
!----------------------------------------------------------------------

    ! compute LAI based on dynamic vegetation option
    if ( CropType == 0 ) then

       ! no dynamic vegetation, use table LAI
       if ( (OptDynamicVeg == 1) .or. (OptDynamicVeg == 3) .or. (OptDynamicVeg == 4) ) then
          if ( Latitude >= 0.0 ) then
            ! Northern Hemisphere
            DAY = DayJulianInYear
          else
            ! Southern Hemisphere.  DAY is shifted by 1/2 year.
            DAY = mod( DayJulianInYear+(0.5*NumDayInYear), real(NumDayInYear) )
          endif
          ! interpolate from montly to target time point
          T   = 12.0 * DAY / real(NumDayInYear)
          IT1 = T + 0.5
          IT2 = IT1 + 1
          WT1 = (IT1 + 0.5) - T
          WT2 = 1.0 - WT1
          if ( IT1 <  1 ) IT1 = 12
          if ( IT2 > 12 ) IT2 = 1
          LAI = WT1 * LAIM(IT1) + WT2 * LAIM(IT2)
          SAI = WT1 * SAIM(IT1) + WT2 * SAIM(IT2)
       endif

       ! no dynamic vegetation, use input LAI time series
       if ( (OptDynamicVeg == 7) .or. (OptDynamicVeg == 8) .or. (OptDynamicVeg == 9) ) then
          SAI = max( 0.05, 0.1*LAI )   ! when reading LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
          if ( LAI < 0.05 ) SAI = 0.0  ! if LAI below minimum, make sure SAI = 0
       endif
       if ( SAI < 0.05 ) SAI = 0.0     ! MB: SAI CHECK, change to 0.05 v3.6
       if ( (LAI < 0.05) .or. (SAI == 0.0) ) LAI = 0.0  ! MB: LAI CHECK

       ! for non-vegetation point
       if ( (VegType == IndexWaterPoint) .or. (VegType == IndexBarrenPoint) .or. &
            (VegType == IndexIcePoint  ) .or. (FlagUrban .eqv. .true.) ) then
          LAI = 0.0
          SAI = 0.0
       endif

    endif   ! CropType == 0

    ! vegetation fraction buried by snow
    DB = min( max(SNOWH-HVB,0.0), (HVT-HVB) )
    FB = DB / max( 1.0e-06, (HVT-HVB) )   ! snow buried fraction
    if ( (HVT > 0.0) .and. (HVT <= 1.0) ) then    ! MB: change to 1.0 and 0.2 to reflect changes to HVT in MPTABLE
       SNOWHC = HVT * exp(-SNOWH / 0.2)
       FB     = min(SNOWH, SNOWHC) / SNOWHC
    endif

    ! adjust LAI and SAI bused on snow bury
    ELAI = LAI * (1.0 - FB)
    ESAI = SAI * (1.0 - FB)
    if ( (ESAI < 0.05) .and. (CropType == 0) ) ESAI = 0.0                       ! MB: ESAI CHECK, change to 0.05 v3.6
    if ( ((ELAI < 0.05) .or. (ESAI == 0.0)) .and. (CropType == 0) ) ELAI = 0.0  ! MB: LAI CHECK

    ! set growing season flag
    if ( ((TV > TMIN) .and. (CropType == 0)) .or. &
         ((PlantGrowStage > 2) .and. (PlantGrowStage < 7) .and. (CropType > 0))) then
       IndexGrowSeason = 1.0
    else
       IndexGrowSeason = 0.0
    endif 

    ! compute vegetation fraction
    ! input green vegetation fraction should be consistent with LAI
    ! use FVEG = SHDFAC from input
    if ( (OptDynamicVeg == 1) .or. (OptDynamicVeg == 6) .or. (OptDynamicVeg == 7) ) then
       FVEG = SHDFAC
    ! computed FVEG from LAI & SAI
    elseif ( (OptDynamicVeg == 2) .or. (OptDynamicVeg == 3) .or. (OptDynamicVeg == 8) ) then
       FVEG = 1.0 - exp(-0.52 * (LAI + SAI))
    ! use yearly maximum vegetation fraction
    elseif ( (OptDynamicVeg == 4) .or. (OptDynamicVeg == 5) .or. (OptDynamicVeg == 9) ) then
       FVEG = SHDMAX
    ! outside existing vegetation options
    else
       write(*,*) "Un-recognized dynamic vegetation option (OptDynamicVeg)... "
       !call wrf_error_fatal("Namelist parameter OptDynamicVeg unknown") 
       stop "error"
    endif
    ! use maximum vegetation fraction for crop run
    if ( (OptCropModel > 0) .and. (CropType > 0) ) then
       FVEG = SHDMAX
    endif

    ! adjust unreasonable vegetation fraction
    if ( FVEG <= 0.05 ) FVEG = 0.05
    if ( (FlagUrban .eqv. .true.) .or. (VegType == IndexBarrenPoint) ) FVEG = 0.0
    if ( (ELAI+ESAI) == 0.0 ) FVEG = 0.0

    ! determine if activate dynamic vegetation or crop run
    FlagDynamicCrop = .false.
    FlagDynamicVeg = .false.
    if ( (OptDynamicVeg == 2) .or. (OptDynamicVeg == 5) .or. (OptDynamicVeg == 6) ) FlagDynamicVeg = .true.
    if ( (OptCropModel > 0) .and. (CropType > 0) ) then
       FlagDynamicCrop = .true.
       FlagDynamicVeg = .false.
    endif

    end associate

  end subroutine PhenologyMain

end module PhenologyMainMod
