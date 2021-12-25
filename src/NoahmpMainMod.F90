module NoahmpMainMod

!!! Main NoahMP module including all column model processes
!!! atmos forcing -> canopy intercept -> precip heat advect -> main energy -> main water -> main carbon -> balance check

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationTriggerMod,       only : IrrigationTrigger
  use IrrigationSprinklerMod,     only : SprinklerIrrigation
  use CanopyWaterInterceptMod,    only : CanopyWaterIntercept
  use PrecipitationHeatAdvectMod, only : PrecipitationHeatAdvect
  use EnergyMainMod,              only : EnergyMain
  use WaterMainMod,               only : WaterMain
 
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

! --------------------------------------------------------------------
    associate(                                                        &
              DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
              CROPLU          => noahmp%config%domain%CROPLU         ,& ! in,     flag to identify croplands
              IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,     irrigation fraction parameter
              IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     irrigation fraction
              IR_RAIN         => noahmp%water%param%IR_RAIN          ,& ! inout,  maximum precipitation to stop irrigation trigger
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent (mm)
              SNEQVO          => noahmp%water%state%SNEQVO           ,& ! inout,  snow mass at last time step(mm)
              IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
              IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
              IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
              RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
              IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
              EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprink
              IREVPLOS        => noahmp%water%flux%IREVPLOS          ,& ! inout,  loss of irrigation water to evaporation,sprinkler [m/timestep]
              FIRR            => noahmp%energy%flux%FIRR              & ! inout,  latent heating due to sprinkler evaporation [w/m2]
             )
! ----------------------------------------------------------------------

    !---------------------------------------------------------------------
    ! call atmospheric forcing processing
    !--------------------------------------------------------------------- 



    !---------------------------------------------------------------------
    ! call phenology
    !--------------------------------------------------------------------- 



    !---------------------------------------------------------------------
    ! call irrigation trigger and sprinkler irrigation
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
    ! call canopy water interception and precip heat advection
    !--------------------------------------------------------------------- 

    call CanopyWaterIntercept(noahmp)
    call PrecipitationHeatAdvect(noahmp)

    !---------------------------------------------------------------------
    ! call the main energy routine
    !--------------------------------------------------------------------- 

    call EnergyMain(noahmp)

    !---------------------------------------------------------------------
    ! prepare for water module
    !--------------------------------------------------------------------- 
    SNEQVO  = SNEQV

    !---------------------------------------------------------------------
    ! call the main water routine
    !--------------------------------------------------------------------- 

    call WaterMain(noahmp)

    !---------------------------------------------------------------------
    ! call the main biochem and crop routine
    !--------------------------------------------------------------------- 


    !---------------------------------------------------------------------
    ! call the main ERROR balance check  routine
    !--------------------------------------------------------------------- 



    end associate

  end subroutine NoahmpMain

end module NoahmpMainMod
