module IrrigationTriggerMod

!!! Trigger irrigation if soil moisture less than the management allowable deficit (MAD)
!!! and estimate irrigation water depth (m) using current rootzone soil moisture and field 
!!! capacity. There are two options here to trigger the irrigation scheme based on MAD
!!! OptIrrigation = 1 -> if irrigated fraction > threshold fraction
!!! OptIrrigation = 2 -> if irrigated fraction > threshold fraction and within crop season
!!! OptIrrigation = 3 -> if irrigated fraction > threshold fraction and LAI > threshold LAI

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine IrrigationTrigger(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TRIGGER_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    logical                :: IRR_ACTIVE  ! irrigation check
    integer                :: K           ! loop index  
    real(kind=kind_noahmp) :: SMCAVL      ! available soil moisture [m] at timestep
    real(kind=kind_noahmp) :: SMCLIM      ! maximum available moisture [m] (FC-PWD)
    real(kind=kind_noahmp) :: SMCSAT      ! maximum saturation moisture [m] (POROSITY-FC)
    real(kind=kind_noahmp) :: IRRWATAMT   ! irrigation water amount [m]

! --------------------------------------------------------------------
    associate(                                                        &
              DepthSoilLayer      => noahmp%config%domain%DepthSoilLayer      ,& ! in,    depth [m] of layer-bottom from soil surface
              DayJulianInYear          => noahmp%config%domain%DayJulianInYear         ,& ! in,     Julian day of the year
              OptIrrigation   => noahmp%config%nmlist%OptIrrigation  ,& ! in,     irrigation option
              OptIrrigationMethod => noahmp%config%nmlist%OptIrrigationMethod ,& ! in,     irrigation method option
              DatePlanting          => noahmp%biochem%param%DatePlanting         ,& ! in,     Planting day (day of year)
              DateHarvest           => noahmp%biochem%param%DateHarvest          ,& ! in,     Harvest date (day of year)
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,     wilting point soil moisture [m3/m3]
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,     reference soil moisture (field capacity) (m3/m3)
              NROOT           => noahmp%water%param%NROOT            ,& ! in,     number of soil layers with root present
              IRR_HAR         => noahmp%water%param%IRR_HAR          ,& ! in,     number of days before harvest date to stop irrigation
              IRR_LAI         => noahmp%water%param%IRR_LAI          ,& ! in,     minimum lai to trigger irrigation
              IRR_MAD         => noahmp%water%param%IRR_MAD          ,& ! in,     management allowable deficit (0-1)
              FILOSS          => noahmp%water%param%FILOSS           ,& ! in,     factor of flood irrigation loss
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,     greeness vegetation fraction (-)
              LAI             => noahmp%energy%state%LAI             ,& ! in,     leaf area index (m2/m2)
              IrrigationFracGrid          => noahmp%water%state%IrrigationFracGrid           ,& ! in,     irrigated area fraction of a grid
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,     soil water content [m3/m3]
              IrrigationFracMicro           => noahmp%water%state%IrrigationFracMicro            ,& ! in,     fraction of grid under micro irrigation (0 to 1)
              IrrigationFracFlood           => noahmp%water%state%IrrigationFracFlood            ,& ! in,     fraction of grid under flood irrigation (0 to 1)
              IrrigationFracSprinkler           => noahmp%water%state%IrrigationFracSprinkler            ,& ! in,     sprinkler irrigation fraction (0 to 1)
              IrrigationAmtMicro         => noahmp%water%state%IrrigationAmtMicro          ,& ! inout,  irrigation water amount [m] to be applied, Micro
              IrrigationAmtFlood         => noahmp%water%state%IrrigationAmtFlood          ,& ! inout,  irrigation water amount [m] to be applied, Flood
              IrrigationAmtSprinkler         => noahmp%water%state%IrrigationAmtSprinkler          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
              IrrigationCntSprinkler         => noahmp%water%state%IrrigationCntSprinkler          ,& ! inout,  irrigation event number, Sprinkler
              IrrigationCntMicro         => noahmp%water%state%IrrigationCntMicro          ,& ! inout,  irrigation event number, Micro
              IrrigationCntFlood         => noahmp%water%state%IrrigationCntFlood           & ! inout,  irrigation event number, Flood
             )
! ----------------------------------------------------------------------

    IRR_ACTIVE = .true.

    ! check if irrigation is can be activated or not
    if ( OptIrrigation == 2 ) then ! activate irrigation if within crop season
       if ( (DayJulianInYear < DatePlanting) .or. (DayJulianInYear > (DateHarvest-IRR_HAR)) ) IRR_ACTIVE = .false.
    elseif ( OptIrrigation == 3) then ! activate if LAI > threshold LAI
       if ( LAI < IRR_LAI) IRR_ACTIVE = .false.
    elseif ( (OptIrrigation > 3) .or. (OptIrrigation < 1) ) then
       IRR_ACTIVE = .false.
    endif

    if ( IRR_ACTIVE .eqv. .true. ) then
       ! estimate available water and field capacity for the root zone
       SMCAVL = 0.0
       SMCLIM = 0.0
       SMCAVL = ( SoilLiqWater(1) - SMCWLT(1) ) * (-1.0) * DepthSoilLayer(1)    ! current soil water (m) 
       SMCLIM = ( SMCREF(1) - SMCWLT(1) ) * (-1.0) * DepthSoilLayer(1)  ! available water (m)
       do K = 2, NROOT
         SMCAVL = SMCAVL + ( SoilLiqWater(K) - SMCWLT(K) ) * ( DepthSoilLayer(K-1) - DepthSoilLayer(K) )
         SMCLIM = SMCLIM + ( SMCREF(K) - SMCWLT(K) ) * ( DepthSoilLayer(K-1) - DepthSoilLayer(K) )
       enddo

      ! check if root zone soil moisture < IRR_MAD (calibratable)
      if ( (SMCAVL/SMCLIM) <= IRR_MAD ) then
         ! amount of water need to be added to bring soil moisture back to 
         ! field capacity, i.e., irrigation water amount (m)
         IRRWATAMT = ( SMCLIM - SMCAVL ) * IrrigationFracGrid * FVEG

         ! sprinkler irrigation amount (m) based on 2D IrrigationFracSprinkler
         if ( (IrrigationAmtSprinkler == 0.0) .and. (IrrigationFracSprinkler > 0.0) .and. (OptIrrigationMethod == 0) ) then
            IrrigationAmtSprinkler = IrrigationFracSprinkler * IRRWATAMT
            IrrigationCntSprinkler = IrrigationCntSprinkler + 1
         ! sprinkler irrigation amount (m) based on namelist choice
         elseif ( (IrrigationAmtSprinkler == 0.0) .and. (OptIrrigationMethod == 1) ) then
            IrrigationAmtSprinkler = IRRWATAMT
            IrrigationCntSprinkler = IrrigationCntSprinkler + 1
         endif

         ! micro irrigation amount (m) based on 2D IrrigationFracMicro
         if ( (IrrigationAmtMicro == 0.0) .and. (IrrigationFracMicro > 0.0) .and. (OptIrrigationMethod == 0) ) then
            IrrigationAmtMicro = IrrigationFracMicro * IRRWATAMT
            IrrigationCntMicro = IrrigationCntMicro + 1
         ! micro irrigation amount (m) based on namelist choice
         elseif ( (IrrigationAmtMicro == 0.0) .and. (OptIrrigationMethod == 2) ) then
            IrrigationAmtMicro = IRRWATAMT
            IrrigationCntMicro = IrrigationCntMicro + 1
         endif

         ! flood irrigation amount (m): Assumed to saturate top two layers and 
         ! third layer to FC. As water moves from one end of the field to
         ! another, surface layers will be saturated. 
         ! flood irrigation amount (m) based on 2D IrrigationFracFlood
         if ( (IrrigationAmtFlood == 0.0) .and. (IrrigationFracFlood > 0.0) .and. (OptIrrigationMethod == 0) ) then
            IrrigationAmtFlood = IrrigationFracFlood * IRRWATAMT * (FILOSS + 1)
            IrrigationCntFlood = IrrigationCntFlood + 1
         !flood irrigation amount (m) based on namelist choice
         elseif ( (IrrigationAmtFlood == 0.0) .and. (OptIrrigationMethod == 3) ) then
            IrrigationAmtFlood = IRRWATAMT * (FILOSS + 1)
            IrrigationCntFlood = IrrigationCntFlood + 1
         endif
      else
         IRRWATAMT = 0.0
         IrrigationAmtSprinkler   = 0.0
         IrrigationAmtMicro   = 0.0
         IrrigationAmtFlood   = 0.0
      endif

    endif

    end associate

  end subroutine IrrigationTrigger

end module IrrigationTriggerMod
