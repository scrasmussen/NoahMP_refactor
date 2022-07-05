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
              T2M              => noahmp%energy%state%T2M           ,& ! in,    2-m air temperature (K)
              PLTDAY           => noahmp%biochem%param%PLTDAY       ,& ! in,    Planting day (day of year)
              HSDAY            => noahmp%biochem%param%HSDAY        ,& ! in,    Harvest date (day of year)
              GDDTBASE         => noahmp%biochem%param%GDDTBASE     ,& ! in,    Base temperature for GDD accumulation [C]
              GDDTCUT          => noahmp%biochem%param%GDDTCUT      ,& ! in,    Upper temperature for GDD accumulation [C]
              GDDS1            => noahmp%biochem%param%GDDS1        ,& ! in,    GDD from seeding to emergence
              GDDS2            => noahmp%biochem%param%GDDS2        ,& ! in,    GDD from seeding to initial vegetative
              GDDS3            => noahmp%biochem%param%GDDS3        ,& ! in,    GDD from seeding to post vegetative
              GDDS4            => noahmp%biochem%param%GDDS4        ,& ! in,    GDD from seeding to intial reproductive
              GDDS5            => noahmp%biochem%param%GDDS5        ,& ! in,    GDD from seeding to physical maturity
              GDD              => noahmp%biochem%state%GDD          ,& ! inout, crop growing degree days
              IPA              => noahmp%biochem%state%IPA          ,& ! out,   Planting index index (0=off, 1=on)
              IHA              => noahmp%biochem%state%IHA          ,& ! out,   Havest index (0=on,1=off) 
              PGS              => noahmp%biochem%state%PGS           & ! out,   Plant growth stage (1=S1,2=S2,3=S3)
             )
!------------------------------------------------------------------------

    ! initialize
    TC = T2M - 273.15

    ! Planting and Havest index
    IPA = 1  ! on
    IHA = 1  ! off

    ! turn on/off the planting 
    if ( DayJulianInYear < PLTDAY ) IPA = 0   ! off
        
    ! turn on/off the harvesting
    if ( DayJulianInYear >= HSDAY ) IHA = 0   ! on            

    ! Calculate the growing degree days               
    if ( TC < GDDTBASE ) then
       TDIFF = 0.0
    elseif ( TC >= GDDTCUT ) then
       TDIFF = GDDTCUT - GDDTBASE
    else
       TDIFF = TC - GDDTBASE
    endif
    GDD      = (GDD + TDIFF * MainTimeStep / 86400.0) * IPA * IHA
    GDDDAY   = GDD
      
    ! Decide corn growth stage, based on Hybrid-Maize 
    !   PGS = 1 : Before planting
    !   PGS = 2 : from tassel initiation to silking
    !   PGS = 3 : from silking to effective grain filling
    !   PGS = 4 : from effective grain filling to pysiological maturity 
    !   PGS = 5 : GDDM=1389
    !   PGS = 6 :
    !   PGS = 7 :
    !   PGS = 8 :
    !GDDM = 1389
    !GDDM = 1555
    !GDDSK = 0.41 * GDDM + 145.4 + 150 ! from hybrid-maize 
    !GDDS1 = ((GDDSK - 96) / 38.9 - 4) * 21
    !GDDS1 = 0.77 * GDDSK
    !GDDS3 = GDDSK + 170
    !GDDS3 = 170

    ! compute plant growth stage
    PGS = 1   ! MB: set PGS = 1 (for initialization during growing season when no GDD)  
    if ( GDDDAY > 0.0 )    PGS = 2
    if ( GDDDAY >= GDDS1 ) PGS = 3
    if ( GDDDAY >= GDDS2 ) PGS = 4 
    if ( GDDDAY >= GDDS3 ) PGS = 5
    if ( GDDDAY >= GDDS4 ) PGS = 6
    if ( GDDDAY >= GDDS5 ) PGS = 7
    if ( DayJulianInYear >= HSDAY ) PGS = 8
    if ( DayJulianInYear < PLTDAY ) PGS = 1   

    end associate

  end subroutine CropGrowDegreeDay

end module CropGrowDegreeDayMod
