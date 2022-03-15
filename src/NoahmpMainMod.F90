module NoahmpMainMod

!!! Main NoahMP module including all column model processes
!!! atmos forcing -> canopy intercept -> precip heat advect -> main energy -> main water -> main carbon -> balance check

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use AtmosForcingMod,            only : ProcessAtmosForcing
  use PhenologyMainMod,           only : PhenologyMain
  use IrrigationTriggerMod,       only : IrrigationTrigger
  use IrrigationSprinklerMod,     only : SprinklerIrrigation
  use CanopyWaterInterceptMod,    only : CanopyWaterIntercept
  use PrecipitationHeatAdvectMod, only : PrecipitationHeatAdvect
  use EnergyMainMod,              only : EnergyMain
  use WaterMainMod,               only : WaterMain
  use BiochemNatureVegMainMod,    only : BiochemNatureVegMain
  use BiochemCropMainMod,         only : BiochemCropMain
 
  implicit none

contains

  subroutine NoahmpMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: NOAHMP_SFLX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IZ        ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              CROPTYPE        => noahmp%config%domain%CROPTYP        ,& ! in,     crop type
              DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
              CROPLU          => noahmp%config%domain%CROPLU         ,& ! in,     flag to identify croplands
              DVEG_ACTIVE     => noahmp%config%domain%DVEG_ACTIVE    ,& ! in,     flag to activate dynamic vegetation model
              CROP_ACTIVE     => noahmp%config%domain%CROP_ACTIVE    ,& ! in,     flag to activate dynamic crop model
              OPT_CROP        => noahmp%config%nmlist%OPT_CROP       ,& ! in,     crop option
              HVB             => noahmp%energy%param%HVB             ,& ! in,     bottom of canopy (m)
              HVT             => noahmp%energy%param%HVT             ,& ! in,     top of canopy (m)
              IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,     irrigation fraction parameter
              IR_RAIN         => noahmp%water%param%IR_RAIN          ,& ! in,     maximum precipitation to stop irrigation trigger
              IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     irrigation fraction
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! inout,  depth of snow/soil layer-bottom (m)
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent (mm)
              SNEQVO          => noahmp%water%state%SNEQVO           ,& ! inout,  snow mass at last time step(mm)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
              IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
              IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
              IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
              SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
              IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
              EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprink
              IREVPLOS        => noahmp%water%flux%IREVPLOS          ,& ! inout,  loss of irrigation water to evaporation,sprinkler [m/timestep]
              FIRR            => noahmp%energy%flux%FIRR             ,& ! inout,  latent heating due to sprinkler evaporation [w/m2]
              LAI             => noahmp%energy%state%LAI             ,& ! inout,  leaf area index (m2/m2)
              SAI             => noahmp%energy%state%SAI             ,& ! inout,  stem area index (m2/m2)
              FB_snow         => noahmp%energy%state%FB_snow         ,& ! out,    fraction of canopy buried by snow
              ELAI            => noahmp%energy%state%ELAI            ,& ! out,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! out,    stem area index, after burying by snow
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! out,    latent heat of vaporization/subli (j/kg), ground
              FGEV            => noahmp%energy%flux%FGEV             ,& ! out,    soil evap heat (w/m2) [+ to atm]
              QVAP            => noahmp%water%flux%QVAP              ,& ! out,    soil surface evaporation rate[mm/s]
              QDEW            => noahmp%water%flux%QDEW              ,& ! out,    soil surface dew rate[mm/s]
              EDIR            => noahmp%water%flux%EDIR               & ! out,    net direct soil evaporation (mm/s)
             )
! ----------------------------------------------------------------------

    !---------------------------------------------------------------------
    ! Atmospheric forcing processing
    !--------------------------------------------------------------------- 

    call ProcessAtmosForcing(noahmp)

    !---------------------------------------------------------------------
    ! Initialize key soil variables
    !--------------------------------------------------------------------- 

    ! snow/soil layer thickness (m)
    do IZ = ISNOW+1, NSOIL
       if ( IZ == ISNOW+1 ) then
          DZSNSO(IZ) = - ZSNSO(IZ)
       else
          DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
       endif
    enddo



    !---------------------------------------------------------------------
    ! Phenology
    !--------------------------------------------------------------------- 

    call PhenologyMain (noahmp)

    !---------------------------------------------------------------------
    ! Irrigation trigger and sprinkler irrigation
    !--------------------------------------------------------------------- 

    if ( (CROPLU .eqv. .true.) .and. (IRRFRA >= IRR_FRAC) .and. (RAIN < (IR_RAIN/3600.0)) .and. &
         ((IRAMTSI+IRAMTMI+IRAMTFI) == 0.0) ) then
       call IrrigationTrigger(noahmp)
    endif
    ! set irrigation off if larger than IR_RAIN mm/h for this time step and irr triggered last time step
    if ( (RAIN >= (IR_RAIN/3600.0)) .or. (IRRFRA < IRR_FRAC) ) then
        IRAMTSI = 0.0
        IRAMTMI = 0.0
        IRAMTFI = 0.0
    endif

    ! call sprinkler irrigation before CANWAT/PRECIP_HEAT to have canopy interception
    if ( (CROPLU .eqv. .true.) .and. (IRAMTSI > 0.0) ) then
       call SprinklerIrrigation(noahmp)
       RAIN = RAIN + (IRSIRATE * 1000.0 / DT) ![mm/s]
       ! cooling and humidification due to sprinkler evaporation, per m^2 calculation 
       FIRR = IREVPLOS * 1000.0 * HVAP / DT   ! heat used for evaporation (W/m2)
       EIRR = IREVPLOS * 1000.0 / DT          ! sprinkler evaporation (mm/s)
    endif

    !---------------------------------------------------------------------
    ! Canopy water interception and precip heat advection
    !--------------------------------------------------------------------- 

    call CanopyWaterIntercept(noahmp)
    call PrecipitationHeatAdvect(noahmp)

    !---------------------------------------------------------------------
    ! Energy processes
    !--------------------------------------------------------------------- 

    call EnergyMain(noahmp)

    !---------------------------------------------------------------------
    ! prepare for water process
    !--------------------------------------------------------------------- 
    SICE(:) = max(0.0, SMC(:)-SH2O(:))
    SNEQVO  = SNEQV
    QVAP    = max(FGEV/LATHEAG, 0.0)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW    = abs(min(FGEV/LATHEAG, 0.0))  ! negative part of fgev
    EDIR    = QVAP - QDEW

    !---------------------------------------------------------------------
    ! Water processes
    !--------------------------------------------------------------------- 

    call WaterMain(noahmp)

    !---------------------------------------------------------------------
    ! Biochem processes (crop and carbon)
    !--------------------------------------------------------------------- 

    ! for natural vegetation
    if ( DVEG_ACTIVE .eqv. .true. ) call BiochemNatureVegMain(noahmp)

    ! for crop
    if ( (OPT_CROP == 1) .and. (CROP_ACTIVE .eqv. .true.) ) call BiochemCropMain(noahmp)

    !---------------------------------------------------------------------
    ! Balance check for energy and water
    !--------------------------------------------------------------------- 



    end associate

  end subroutine NoahmpMain

end module NoahmpMainMod
