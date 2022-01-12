module GrowingDegreeDaysCropMod

    !!! Estimate Growing Degree Days
    use Machine, only : kind_noahmp
    use NoahmpVarType
        
    implicit none

contains

    subroutine GrowingDegreeDaysCrop (noahmp)

        ! ------------------------ Code history -----------------------------------
        ! Original Noah-MP subroutine: GROWING_GDD 
        ! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
        ! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
        ! -------------------------------------------------------------------------
        
        implicit none
       
        type(noahmp_type), intent(inout) :: noahmp

        real(kind=kind_noahmp)           :: GDDDAY    ! gap bewtween GDD and GDD8
        real(kind=kind_noahmp)           :: DAYOFS2   ! DAYS in stage2
        real(kind=kind_noahmp)           :: TDIFF     ! temperature difference for growing degree days calculation
        real(kind=kind_noahmp)           :: TC

        associate(                                            &
                 T2M       =>  noahmp%energy%state%T2M       ,&
                 DT        =>  noahmp%config%domain%DT       ,& 
                 JULIAN    =>  noahmp%config%domain%YEARLEN  ,&
                 GDD       =>  noahmp%biochem%state%GDD      ,& 
                 IPA       =>  noahmp%biochem%state%IPA      ,& 
                 IHA       =>  noahmp%biochem%state%IHA      ,&
                 PGS       =>  noahmp%biochem%state%PGS      ,&
                 PLTDAY    =>  noahmp%biochem%param%PLTDAY   ,&
                 HSDAY     =>  noahmp%biochem%param%HSDAY    ,&
                 GDDTBASE  =>  noahmp%biochem%param%GDDTBASE ,&
                 GDDTCUT   =>  noahmp%biochem%param%GDDTCUT  ,& 
                 GDDS1     =>  noahmp%biochem%param%GDDS1    ,&
                 GDDS2     =>  noahmp%biochem%param%GDDS2    ,&
                 GDDS3     =>  noahmp%biochem%param%GDDS3    ,&
                 GDDS4     =>  noahmp%biochem%param%GDDS4    ,&
                 GDDS5     =>  noahmp%biochem%param%GDDS5     &
                 )

        TC = T2M - 273.15

        ! Havestindex(0=on,1=off) 
        IPA = 1
        IHA = 1
        
        !turn on/off the planting 
                  
        if (JULIAN < PLTDAY)  IPA = 0
        
        !turn on/off the harvesting
        
        if (JULIAN >= HSDAY) IHA = 0
                    
        ! Calculate the growing degree days
                   
        if (TC <  GDDTBASE) then
            TDIFF = 0.0
        elseif (TC >= GDDTCUT) then
            TDIFF = GDDTCUT - GDDTBASE
        else
            TDIFF = TC - GDDTBASE
        endif

        GDD     = (GDD + TDIFF * DT / 86400.0) * IPA * IHA
        GDDDAY  = GDD

        ! Decide corn growth stage, based on Hybrid-Maize 
        !   PGS = 1 : Before planting
        !   PGS = 2 : from tassel initiation to silking
        !   PGS = 3 : from silking to effective grain filling
        !   PGS = 4 : from effective grain filling to pysiological maturity 
        !   PGS = 5 : GDDM=1389
        !   PGS = 6 :
        !   PGS = 7 :
        !   PGS = 8 :
        !  GDDM = 1389
        !  GDDM = 1555
        ! GDDSK = 0.41*GDDM +145.4+150 !from hybrid-maize 
        ! GDDS1 = ((GDDSK-96)/38.9-4)*21
        ! GDDS1 = 0.77*GDDSK
        ! GDDS3 = GDDSK+170
        ! GDDS3 = 170

        PGS = 1                         ! MB: set PGS = 1 (for initialization during growing season when no GDD)
                
        IF(GDDDAY > 0.0) PGS = 2
        
        IF(GDDDAY >= GDDS1)  PGS = 3
        
        IF(GDDDAY >= GDDS2)  PGS = 4 
        
        IF(GDDDAY >= GDDS3)  PGS = 5
        
        IF(GDDDAY >= GDDS4)  PGS = 6
        
        IF(GDDDAY >= GDDS5)  PGS = 7
        
        IF(JULIAN >= HSDAY)  PGS = 8

        IF(JULIAN <  PLTDAY) PGS = 1   


        end associate

    end subroutine GrowingDegreeDaysCrop


end module GrowingDegreeDaysCropMod