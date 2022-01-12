module CarbonAssimilationFluxMod

    !!! Main Carbon Assimilation for Vegetation
        
    use Machine, only : kind_noahmp
    use NoahmpVarType
        
    implicit none
        
contains

    subroutine CarbonAssimilationFlux (noahmp)

        ! ------------------------ Code history -----------------------------------
        ! Original Noah-MP subroutine: CO2FLUX
        ! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
        ! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
        ! -------------------------------------------------------------------------
        
        implicit none
        
        type(noahmp_type), intent(inout) :: noahmp

        associate(
                 ILOC     =>  noahmp%config%domain%ILOC       ,&
                 JLOC     =>  noahmp%config%domain%JLOC       ,& 
                 VEGTYP   =>  noahmp%config%domain%VEGTYP     ,&   
                 NSNOW    =>  noahmp%config%domain%NSNOW      ,&  
                 NSOIL    =>  noahmp%config%domain%NSOIL      ,&
                 DT       =>  noahmp%config%domain%DT         ,&
                 LAT      =>  noahmp%config%domain%LAT        ,&
                 IGS      =>  noahmp%biochem%state%IGS        ,&
                 DZSNSO   =>  noahmp%config%domain%DZSNSO     ,& 
                 STC      =>  noahmp%energy%state%STC         ,&
                 PSN      =>  noahmp%biochem%flux%PSN         ,&
                 TROOT    =>  noahmp%energy%state%TROOT       ,&
                 TV       =>  noahmp%energy%state%TV          ,&
                 WROOT    =>  noahmp%water%state%WROOT        ,&
                 WSTRES   =>  noahmp%water%state%WSTRES       ,&
                 FOLN     =>  noahmp%biochem%state%FOLN       ,&
                 LAPM     =>  noahmp%biochem%state%LAPM       ,&
                 FVEG     =>  noahmp%energy%state%FVEG        ,&
                 XLAI     =>  noahmp%energy%state%LAI         ,&
                 XSAI     =>  noahmp%energy%state%SAI         ,&
                 LFMASS   =>  noahmp%biochem%state%LFMASS     ,&  
                 RTMASS   =>  noahmp%biochem%state%RTMASS     ,&
                 STMASS   =>  noahmp%biochem%state%STMASS     ,&
                 FASTCP   =>  noahmp%biochem%state%FASTCP     ,&
                 STBLCP   =>  noahmp%biochem%state%STBLCP     ,&
                 WOOD     =>  noahmp%biochem%state%WOOD       ,&
                 GPP      =>  noahmp%biochem%flux%GPP         ,&
                 NPP      =>  noahmp%biochem%flux%NPP         ,&
                 NEE      =>  noahmp%biochem%flux%NEE         ,&
                 AUTORS   =>  noahmp%biochem%flux%AUTORS      ,&
                 HETERS   =>  noahmp%biochem%flux%HETERS      ,&
                 TOTSC    =>  noahmp%biochem%state%TOTSC      ,&
                 TOTLB    =>  noahmp%biochem%state%TOTLB      ,&
                 CFLUX    =>  noahmp%biochem%flux%CFLUX       ,&
                 LFMSMN   =>  noahmp%biochem%state%LFMSMN     ,&
                 RSWOOD   =>  noahmp%biochem%state%RSWOOD     ,&
                 RSLEAF   =>  noahmp%biochem%state%RSLEAF     ,&
                 RSROOT   =>  noahmp%biochem%state%RSROOT     ,&
                 NPPL     =>  noahmp%biochem%flux%NPPL        ,&
                 NPPR     =>  noahmp%biochem%flux%NPPR        ,&
                 NPPW     =>  noahmp%biochem%flux%NPPW        ,&
                 NPPS     =>  noahmp%biochem%flux%NPPS        ,&
                 DIELF    =>  noahmp%biochem%state%DIELF      ,&
                 ADDNPPLF =>  noahmp%biochem%state%ADDNPPLF   ,&
                 ADDNPPST =>  noahmp%biochem%state%ADDNPPST   ,&
                 CARBFX   =>  noahmp%biochem%state%CARBFX     ,&
                 GRLEAF   =>  noahmp%biochem%flux%GRLEAF      ,&
                 GRROOT   =>  noahmp%biochem%flux%GRROOT      ,&
                 GRWOOD   =>  noahmp%biochem%flux%GRWOOD      ,&
                 GRSTEM   =>  noahmp%biochem%flux%GRSTEM      ,&
                 LEAFPT   =>  noahmp%biochem%state%LEAFPT     ,&
                 LFDEL    =>  noahmp%biochem%flux%LFDEL       ,&
                 LFTOVR   =>  noahmp%biochem%state%LFTOVR     ,&
                 STTOVR   =>  noahmp%biochem%state%STTOVR     ,&
                 WDTOVR   =>  noahmp%biochem%state%WDTOVR     ,&
                 RSSOIL   =>  noahmp%biochem%state%RSSOIL     ,&
                 RTTOVR   =>  noahmp%biochem%state%RTTOVR     ,&
                 STABLC   =>  noahmp%biochem%flux%STABLC      ,&
                 WOODF    =>  noahmp%biochem%state%WOODF      ,&
                 NONLEF   =>  noahmp%biochem%state%NONLEF     ,&
                 ROOTPT   =>  noahmp%biochem%state%ROOTPT     ,&
                 WOODPT   =>  noahmp%biochem%state%WOODPT     ,&
                 STEMPT   =>  noahmp%biochem%state%STEMPT     ,&
                 RESP     =>  noahmp%biochem%flux%RESP        ,&
                 RSSTEM   =>  noahmp%biochem%flux%RSSTEM      ,&
                 FSW      =>  noahmp%biochem%state%FSW        ,&
                 FST      =>  noahmp%biochem%state%FST        ,&
                 FNF      =>  noahmp%biochem%state%FNF        ,&
                 TF       =>  noahmp%biochem%state%TF         ,&
                 RF       =>  noahmp%biochem%state%RF         ,&
                 STDEL    =>  noahmp%biochem%flux%STDEL       ,&
                 STMSMN   =>  noahmp%biochem%state%STMSMN     ,&
                 SAPM     =>  noahmp%biochem%state%SAPM       ,&
                 DIEST    =>  noahmp%biochem%state%DIEST      ,&
                 FOLNMX   =>  noahmp%biochem%param%FOLNMX     ,&
                 ARM      =>  noahmp%biochem%param%ARM        ,&
                 RMF25    =>  noahmp%biochem%param%RMF25      ,&
                 RMR25    =>  noahmp%biochem%param%RMR25      ,&
                 RMS25    =>  noahmp%biochem%param%RMS25      ,&
                 WDPOOL   =>  noahmp%biochem%param%WDPOOL     ,&
                 EBLFOREST=>  noahmp%config%domain%EBLFOREST  ,& 
                 WRRAT    =>  noahmp%biochem%param%WRRAT      ,&
                 LTOVRC   =>  noahmp%biochem%param%LTOVRC     ,&
                 TDLEF    =>  noahmp%biochem%param%TDLEF      ,&
                 DILEFW   =>  noahmp%biochem%param%DILEFW     ,&
                 DILEFC   =>  noahmp%biochem%param%DILEFC     ,&
                 FRAGR    =>  noahmp%biochem%param%FRAGR      ,&
                 TMIN     =>  noahmp%biochem%param%TMIN       ,&
                 MRP      =>  noahmp%biochem%param%MRP         &
                )

      
        ! local variable
        real(kind=kind_noahmp)           :: BF       !parameter for present wood allocation [-]
        real(kind=kind_noahmp)           :: RSWOODC  !wood respiration coeficient [1/s]
        real(kind=kind_noahmp)           :: STOVRC   !stem turnover coefficient [1/s]
        real(kind=kind_noahmp)           :: RSDRYC   !degree of drying that reduces soil respiration [-]
        real(kind=kind_noahmp)           :: RTOVRC   !root turnover coefficient [1/s]
        real(kind=kind_noahmp)           :: WSTRC    !water stress coeficient [-]
        real(kind=kind_noahmp)           :: LAIMIN   !minimum leaf area index [m2/m2]
        real(kind=kind_noahmp)           :: XSAMIN   !minimum leaf area index [m2/m2]
        real(kind=kind_noahmp)           :: SC
        real(kind=kind_noahmp)           :: SD
        real(kind=kind_noahmp)           :: VEGFRAC

        integer                          :: J

        ! Respiration as a function of temperature
        real :: r,x
        r(x) = exp(0.08*(x-298.16))

        ! ---------------------------------------------------------------------------------
        ! constants
        RTOVRC  = 2.0E-8        ! original was 2.0e-8
        RSDRYC  = 40.0          ! original was 40.0
        RSWOODC = 3.0E-10       
        BF      = 0.90          ! original was 0.90   ! carbon to roots
        WSTRC   = 100.0
        LAIMIN  = 0.05   
        XSAMIN  = 0.05          ! MB: change to prevent vegetation from not growing back in spring
        SAPM    = 3.*0.001      ! m2/kg -->m2/g
        LFMSMN  = laimin/lapm
        STMSMN  = xsamin/sapm
        ! ---------------------------------------------------------------------------------

        ! respiration

        if (IGS .EQ. 0.0) then
           RF = 0.5
        else
           RF = 1.0
        endif
             
        FNF     = MIN( FOLN/MAX(1.0E-06,FOLNMX), 1.0 )
        TF      = ARM**( (TV-298.16)/10.0 )
        RESP    = RMF25 * TF * FNF * XLAI * RF * (1.-WSTRES)        ! umol/m2/s
        RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*12.0e-6)              ! g/m2/s
      
        RSROOT  = RMR25*(RTMASS*1E-3)*TF *RF* 12.0e-6               ! g/m2/s
        RSSTEM  = RMS25*((STMASS-STMSMN)*1E-3)*TF *RF* 12.0e-6      ! g/m2/s
        RSWOOD  = RSWOODC * R(TV) * WOOD*WDPOOL

        ! carbon assimilation
        ! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;

        CARBFX  = PSN * 12.0e-6                                     ! umol co2 /m2/ s -> g/m2/s carbon

        ! fraction of carbon into leaf versus nonleaf

        LEAFPT = EXP(0.01*(1.-EXP(0.75*XLAI))*XLAI)
        if (VEGTYP == EBLFOREST) LEAFPT = EXP(0.01*(1.-EXP(0.50*XLAI))*XLAI)
        NONLEF = 1.0 - LEAFPT
        STEMPT = XLAI/10.0*LEAFPT
        LEAFPT = LEAFPT - STEMPT

        !  fraction of carbon into wood versus root

        if (WOOD > 1.0e-6) then
           WOODF = (1.0-EXP(-BF*(WRRAT*RTMASS/WOOD))/BF)*WDPOOL
        else
           WOODF = WDPOOL
        endif
        
        ROOTPT = NONLEF*(1.0-WOODF)
        WOODPT = NONLEF*WOODF

        ! leaf and root turnover per time step

        LFTOVR = LTOVRC*5.0E-7*LFMASS
        STTOVR = LTOVRC*5.0E-7*STMASS
        RTTOVR = RTOVRC*RTMASS
        WDTOVR = 9.5E-10*WOOD

        ! seasonal leaf die rate dependent on temp and water stress
        ! water stress is set to 1 at permanent wilting point
        
        SC  = EXP(-0.3*MAX(0.0,TV-TDLEF)) * (LFMASS/120.0) 
        SD  = EXP((WSTRES-1.)*WSTRC)
        DIELF = LFMASS*1.0E-6*(DILEFW * SD + DILEFC*SC)
        DIEST = STMASS*1.0E-6*(DILEFW * SD + DILEFC*SC)

        ! calculate growth respiration for leaf, rtmass and wood

        GRLEAF = MAX(0.0,FRAGR*(LEAFPT*CARBFX - RSLEAF))
        GRSTEM = MAX(0.0,FRAGR*(STEMPT*CARBFX - RSSTEM))
        GRROOT = MAX(0.0,FRAGR*(ROOTPT*CARBFX - RSROOT))
        GRWOOD = MAX(0.0,FRAGR*(WOODPT*CARBFX - RSWOOD))

        ! Impose lower T limit for photosynthesis

        ADDNPPLF = MAX(0.0,LEAFPT*CARBFX - GRLEAF-RSLEAF)
        ADDNPPST = MAX(0.0,STEMPT*CARBFX - GRSTEM-RSSTEM)

        !     ADDNPPLF = LEAFPT*CARBFX - GRLEAF-RSLEAF  ! MB: test Kjetil 
        !     ADDNPPST = STEMPT*CARBFX - GRSTEM-RSSTEM  ! MB: test Kjetil 

        if (TV .LT. TMIN) ADDNPPLF =0.0
        if (TV .LT. TMIN) ADDNPPST =0.0

        ! update leaf, root, and wood carbon
        ! avoid reducing leaf mass below its minimum value but conserve mass

        LFDEL = (LFMASS - LFMSMN)/DT
        STDEL = (STMASS - STMSMN)/DT
        DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)
        DIEST = MIN(DIEST,STDEL+ADDNPPST-STTOVR)

        ! net primary productivities

        NPPL   = MAX(ADDNPPLF,-LFDEL)
        NPPS   = MAX(ADDNPPST,-STDEL)
        NPPR   = ROOTPT*CARBFX - RSROOT - GRROOT
        NPPW   = WOODPT*CARBFX - RSWOOD - GRWOOD

        ! masses of plant components

        LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
        STMASS = STMASS + (NPPS-STTOVR-DIEST)*DT   ! g/m2
        RTMASS = RTMASS + (NPPR-RTTOVR)      *DT
        if (RTMASS.LT.0.0) THEN
           RTTOVR = NPPR
           RTMASS = 0.0
        endif
        WOOD = (WOOD+(NPPW-WDTOVR)*DT)*WDPOOL

        ! soil carbon budgets

        FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF+DIEST)*DT  ! MB: add DIEST v3.7
        FST = 2.0**( (STC(1)-283.16)/10.0 )
        FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
        RSSOIL = FSW * FST * MRP* MAX(0.0,FASTCP*1.0E-3)*12.0E-6
        STABLC = 0.1*RSSOIL
        FASTCP = FASTCP - (RSSOIL + STABLC)*DT
        STBLCP = STBLCP + STABLC*DT

        !  total carbon flux
        
        CFLUX  = - CARBFX + RSLEAF + RSROOT + RSWOOD + RSSTEM &     ! MB: add RSSTEM,GRSTEM,0.9*RSSOIL v3.7
                 + 0.9*RSSOIL + GRLEAF + GRROOT + GRWOOD + GRSTEM   ! g/m2/s

        ! for outputs

        GPP    = CARBFX                                             !g/m2/s C
        NPP    = NPPL + NPPW + NPPR +NPPS                           !g/m2/s C
        AUTORS = RSROOT + RSWOOD  + RSLEAF + RSSTEM + &             !g/m2/s C  MB: add RSSTEM, GRSTEM v3.7
                 GRLEAF + GRROOT + GRWOOD + GRSTEM                  !g/m2/s C  MB: add 0.9* v3.7
        HETERS = 0.9*RSSOIL                                         !g/m2/s C
        NEE    = (AUTORS + HETERS - GPP)*44.0/12.0                  !g/m2/s CO2
        TOTSC  = FASTCP + STBLCP                                    !g/m2   C
        TOTLB  = LFMASS + RTMASS +STMASS + WOOD                     !g/m2   C  MB: add STMASS v3.7

        ! leaf area index and stem area index

        XLAI    = MAX(LFMASS*LAPM,LAIMIN)
        XSAI    = MAX(STMASS*SAPM,XSAMIN)
        
        end associate

    end subroutine CarbonAssimilationFlux

end module CarbonAssimilationFluxMod