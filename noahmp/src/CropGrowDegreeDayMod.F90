module CropGrowDegreeDayMod

!!! Compute crop growing degree days

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CropGrowDegreeDay(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROWING_GDD 
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
        
    implicit none
       
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: GDDDAY    ! gap bewtween GDD and GDD8
    real(kind=kind_noahmp)           :: DAYOFS2   ! DAYS in stage2
    real(kind=kind_noahmp)           :: TDIFF     ! temperature difference for growing degree days calculation
    real(kind=kind_noahmp)           :: TC        ! temperature degC

!------------------------------------------------------------------------
    associate(                                                       &
              MainTimeStep     => noahmp%config%domain%MainTimeStep ,& ! in,    main noahmp timestep (s)
              DayJulianInYear           => noahmp%config%domain%DayJulianInYear       ,& ! in,    Julian day of year
              TemperatureAir2m              => noahmp%energy%state%TemperatureAir2m           ,& ! in,    2-m air temperature (K)
              DatePlanting           => noahmp%biochem%param%DatePlanting       ,& ! in,    Planting day (day of year)
              DateHarvest            => noahmp%biochem%param%DateHarvest        ,& ! in,    Harvest date (day of year)
              TempBaseGrowDegDay         => noahmp%biochem%param%TempBaseGrowDegDay     ,& ! in,    Base temperature for grow degree day accumulation [C]
              TempMaxGrowDegDay          => noahmp%biochem%param%TempMaxGrowDegDay      ,& ! in,    Max temperature for grow degree day accumulation [C]
              GrowDegDayEmerg            => noahmp%biochem%param%GrowDegDayEmerg        ,& ! in,    grow degree day from seeding to emergence
              GrowDegDayInitVeg            => noahmp%biochem%param%GrowDegDayInitVeg        ,& ! in,    grow degree day from seeding to initial vegetative
              GrowDegDayPostVeg            => noahmp%biochem%param%GrowDegDayPostVeg        ,& ! in,    grow degree day from seeding to post vegetative
              GrowDegDayInitReprod            => noahmp%biochem%param%GrowDegDayInitReprod        ,& ! in,    grow degree day from seeding to intial reproductive
              GrowDegDayMature            => noahmp%biochem%param%GrowDegDayMature        ,& ! in,    grow degree day from seeding to physical maturity
              GrowDegreeDay    => noahmp%biochem%state%GrowDegreeDay ,& ! inout, crop growing degree days
              IndexPlanting              => noahmp%biochem%state%IndexPlanting          ,& ! out,   Planting index index (0=off, 1=on)
              IndexHarvest              => noahmp%biochem%state%IndexHarvest          ,& ! out,   Havest index (0=on,1=off) 
              PlantGrowStage              => noahmp%biochem%state%PlantGrowStage           & ! out,   Plant growth stage (1=S1,2=S2,3=S3)
             )
!------------------------------------------------------------------------

    ! initialize
    TC = TemperatureAir2m - 273.15

    ! Planting and Havest index
    IndexPlanting = 1  ! on
    IndexHarvest = 1  ! off

    ! turn on/off the planting 
    if ( DayJulianInYear < DatePlanting ) IndexPlanting = 0   ! off
        
    ! turn on/off the harvesting
    if ( DayJulianInYear >= DateHarvest ) IndexHarvest = 0   ! on            

    ! Calculate the growing degree days               
    if ( TC < TempBaseGrowDegDay ) then
       TDIFF = 0.0
    elseif ( TC >= TempMaxGrowDegDay ) then
       TDIFF = TempMaxGrowDegDay - TempBaseGrowDegDay
    else
       TDIFF = TC - TempBaseGrowDegDay
    endif
    GrowDegreeDay = (GrowDegreeDay + TDIFF * MainTimeStep / 86400.0) * IndexPlanting * IndexHarvest
    GDDDAY   = GrowDegreeDay
      
    ! Decide corn growth stage, based on Hybrid-Maize 
    !   PlantGrowStage = 1 : Before planting
    !   PlantGrowStage = 2 : from tassel initiation to silking
    !   PlantGrowStage = 3 : from silking to effective grain filling
    !   PlantGrowStage = 4 : from effective grain filling to pysiological maturity 
    !   PlantGrowStage = 5 : GDDM=1389
    !   PlantGrowStage = 6 :
    !   PlantGrowStage = 7 :
    !   PlantGrowStage = 8 :
    !GDDM = 1389
    !GDDM = 1555
    !GDDSK = 0.41 * GDDM + 145.4 + 150 ! from hybrid-maize 
    !GrowDegDayEmerg = ((GDDSK - 96) / 38.9 - 4) * 21
    !GrowDegDayEmerg = 0.77 * GDDSK
    !GrowDegDayPostVeg = GDDSK + 170
    !GrowDegDayPostVeg = 170

    ! compute plant growth stage
    PlantGrowStage = 1   ! MB: set PlantGrowStage = 1 (for initialization during growing season when no GDD)  
    if ( GDDDAY > 0.0 )    PlantGrowStage = 2
    if ( GDDDAY >= GrowDegDayEmerg ) PlantGrowStage = 3
    if ( GDDDAY >= GrowDegDayInitVeg ) PlantGrowStage = 4 
    if ( GDDDAY >= GrowDegDayPostVeg ) PlantGrowStage = 5
    if ( GDDDAY >= GrowDegDayInitReprod ) PlantGrowStage = 6
    if ( GDDDAY >= GrowDegDayMature ) PlantGrowStage = 7
    if ( DayJulianInYear >= DateHarvest ) PlantGrowStage = 8
    if ( DayJulianInYear < DatePlanting ) PlantGrowStage = 1   

    end associate

  end subroutine CropGrowDegreeDay

end module CropGrowDegreeDayMod
