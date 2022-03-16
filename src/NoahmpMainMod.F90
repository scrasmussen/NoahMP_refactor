module NoahmpMainMod

!!! Main NoahMP module including all column model processes
!!! atmos forcing -> canopy intercept -> precip heat advect -> main energy -> main water -> main biogeochemistry -> balance check

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use AtmosForcingMod,            only : ProcessAtmosForcing
  use PhenologyMainMod,           only : PhenologyMain
  use IrrigationPrepareMod,       only : IrrigationPrepare
  use IrrigationSprinklerMod,     only : IrrigationSprinkler
  use CanopyWaterInterceptMod,    only : CanopyWaterIntercept
  use PrecipitationHeatAdvectMod, only : PrecipitationHeatAdvect
  use EnergyMainMod,              only : EnergyMain
  use WaterMainMod,               only : WaterMain
  use BiochemNatureVegMainMod,    only : BiochemNatureVegMain
  use BiochemCropMainMod,         only : BiochemCropMain
  use BalanceErrorCheckMod,       only : BalanceErrorCheck
 
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
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake
              CROPTYPE        => noahmp%config%domain%CROPTYP        ,& ! in,     crop type
              DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
              DVEG_ACTIVE     => noahmp%config%domain%DVEG_ACTIVE    ,& ! in,     flag to activate dynamic vegetation model
              CROP_ACTIVE     => noahmp%config%domain%CROP_ACTIVE    ,& ! in,     flag to activate dynamic crop model
              OPT_CROP        => noahmp%config%nmlist%OPT_CROP       ,& ! in,     crop option
              HVB             => noahmp%energy%param%HVB             ,& ! in,     bottom of canopy (m)
              HVT             => noahmp%energy%param%HVT             ,& ! in,     top of canopy (m)
              NROOT           => noahmp%water%param%NROOT            ,& ! in,     number of soil layers with root present
              IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,     irrigation fraction parameter
              IR_RAIN         => noahmp%water%param%IR_RAIN          ,& ! in,     maximum precipitation to stop irrigation trigger
              IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     irrigation fraction
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! inout,  depth of snow/soil layer-bottom (m)
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent (mm)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
              IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
              IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
              IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
              SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              CANLIQ          => noahmp%water%state%CANLIQ           ,& ! inout,  intercepted liquid water (mm)
              CANICE          => noahmp%water%state%CANICE           ,& ! inout,  intercepted ice mass (mm)
              WA              => noahmp%water%state%WA               ,& ! inout,  water storage in aquifer [mm]
              RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
              IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
              EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprink
              IREVPLOS        => noahmp%water%flux%IREVPLOS          ,& ! inout,  loss of irrigation water to evaporation,sprinkler [m/timestep]
              FIRR            => noahmp%energy%flux%FIRR             ,& ! inout,  latent heating due to sprinkler evaporation [w/m2]
              LAI             => noahmp%energy%state%LAI             ,& ! inout,  leaf area index (m2/m2)
              SAI             => noahmp%energy%state%SAI             ,& ! inout,  stem area index (m2/m2)
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              CROPLU          => noahmp%config%domain%CROPLU         ,& ! out,    flag to identify croplands
              FB_snow         => noahmp%energy%state%FB_snow         ,& ! out,    fraction of canopy buried by snow
              ELAI            => noahmp%energy%state%ELAI            ,& ! out,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! out,    stem area index, after burying by snow
              TROOT           => noahmp%energy%state%TROOT           ,& ! out,    root-zone averaged temperature (k)
              BEG_WB          => noahmp%water%state%BEG_WB            & ! out,    total water storage at the beginning
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

    ! root-zone soil temperature
    TROOT = 0.0
    do IZ = 1, NROOT
       TROOT = TROOT + STC(IZ) * DZSNSO(IZ) / (-ZSOIL(NROOT))
    enddo

    !---------------------------------------------------------------------
    ! Prepare for water balance check
    !--------------------------------------------------------------------- 

    ! compute total water storage before NoahMP processes
    if ( IST == 1 ) then  ! soil
       BEG_WB = CANLIQ + CANICE + SNEQV + WA
       do IZ = 1, NSOIL
          BEG_WB = BEG_WB + SMC(IZ) * DZSNSO(IZ) * 1000.0
       enddo
    endif

    !---------------------------------------------------------------------
    ! Phenology
    !--------------------------------------------------------------------- 

    call PhenologyMain(noahmp)

    !---------------------------------------------------------------------
    ! Irrigation prepare including trigger
    !--------------------------------------------------------------------- 

    call IrrigationPrepare(noahmp)

    !---------------------------------------------------------------------
    ! Sprinkler irrigation
    !--------------------------------------------------------------------- 

    ! call sprinkler irrigation before canopy process to have canopy interception
    if ( (CROPLU .eqv. .true.) .and. (IRAMTSI > 0.0) ) call IrrigationSprinkler(noahmp)

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
    ! Water processes
    !--------------------------------------------------------------------- 

    call WaterMain(noahmp)

    !---------------------------------------------------------------------
    ! Biochem processes (crop and carbon)
    !--------------------------------------------------------------------- 

    if ( DVEG_ACTIVE .eqv. .true. ) call BiochemNatureVegMain(noahmp)                     ! for natural vegetation
    if ( (OPT_CROP == 1) .and. (CROP_ACTIVE .eqv. .true.) ) call BiochemCropMain(noahmp)  ! for crop

    !---------------------------------------------------------------------
    ! Error check for energy and water balance
    !--------------------------------------------------------------------- 

    call BalanceErrorCheck(noahmp)

    !---------------------------------------------------------------------
    ! End of all NoahMP column processes
    !--------------------------------------------------------------------- 

    end associate

  end subroutine NoahmpMain

end module NoahmpMainMod
