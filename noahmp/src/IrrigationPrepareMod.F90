module IrrigationPrepareMod

!!! Prepare dynamic irrigation variables and trigger irrigation based on conditions

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationTriggerMod, only : IrrigationTrigger

  implicit none

contains

  subroutine IrrigationPrepare(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_SFLX
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! ----------------------------------------------------------------------
    associate(                                                        &
              LandUseDataName        => noahmp%config%domain%LandUseDataName       ,& ! in,     landuse data name (USGS or MODIS_IGBP)
              VegType         => noahmp%config%domain%VegType                 ,& ! in,    vegetation type
              OptIrrigationMethod => noahmp%config%nmlist%OptIrrigationMethod ,& ! in,    irrigation method option
              IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,     irrigation fraction parameter
              IR_RAIN         => noahmp%water%param%IR_RAIN          ,& ! in,     maximum precipitation to stop irrigation trigger
              IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     total input irrigation fraction
              SIFRA           => noahmp%water%state%SIFRA            ,& ! in,     input sprinkler irrigation fraction (0 to 1)
              MIFRA           => noahmp%water%state%MIFRA            ,& ! in,     input micro irrigation fraction (0 to 1)
              FIFRA           => noahmp%water%state%FIFRA            ,& ! in,     input flood irrigation fraction (0 to 1) 
              IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
              IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
              IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
              RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
              FlagCropland          => noahmp%config%domain%FlagCropland         ,& ! out,    flag to identify croplands
              SIFAC           => noahmp%water%state%SIFAC            ,& ! out,    sprinkler irrigation fraction (0 to 1)
              MIFAC           => noahmp%water%state%MIFAC            ,& ! out,    fraction of grid under micro irrigation (0 to 1)
              FIFAC           => noahmp%water%state%FIFAC             & ! out,    fraction of grid under flood irrigation (0 to 1)
             )
! ----------------------------------------------------------------------

    ! initialize
    FlagCropland = .false.

    ! determine cropland
    if ( trim(LandUseDataName) == "USGS" ) then
       if ( (VegType >= 3) .and. (VegType <= 6) ) FlagCropland = .true.
    elseif ( trim(LandUseDataName) == "MODIFIED_IGBP_MODIS_NOAH") then
       if ( (VegType == 12) .or. (VegType == 14) ) FlagCropland = .true.
    endif

    ! assign irrigation fraction from input data 
    SIFAC = SIFRA
    MIFAC = MIFRA
    FIFAC = FIFRA

    ! if OptIrrigationMethod = 0 and if methods are unknown for certain area, then use sprinkler irrigation method
    if ( (OptIrrigationMethod == 0) .and. (SIFAC == 0.0) .and. (MIFAC == 0.0) &
         .and. (FIFAC == 0.0) .and. (IRRFRA >= IRR_FRAC) ) then
       SIFAC = 1.0
    endif

    ! choose method based on user namelist choice
    if ( OptIrrigationMethod == 1 ) then
       SIFAC = 1.0
       MIFAC = 0.0
       FIFAC = 0.0
    elseif ( OptIrrigationMethod == 2 ) then
       SIFAC = 0.0
       MIFAC = 1.0
       FIFAC = 0.0
    elseif ( OptIrrigationMethod == 3 ) then
       SIFAC = 0.0
       MIFAC = 0.0
       FIFAC = 1.0
    endif

    ! trigger irrigation
    if ( (FlagCropland .eqv. .true.) .and. (IRRFRA >= IRR_FRAC) .and. &
         (RAIN < (IR_RAIN/3600.0)) .and. ((IRAMTSI+IRAMTMI+IRAMTFI) == 0.0) ) then
       call IrrigationTrigger(noahmp)
    endif

    ! set irrigation off if larger than IR_RAIN mm/h for this time step and irr triggered last time step
    if ( (RAIN >= (IR_RAIN/3600.0)) .or. (IRRFRA < IRR_FRAC) ) then
        IRAMTSI = 0.0
        IRAMTMI = 0.0
        IRAMTFI = 0.0
    endif

    end associate

  end subroutine IrrigationPrepare

end module IrrigationPrepareMod
