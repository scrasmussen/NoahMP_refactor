module CarbonFluxNatureVegMod

!!! Main Carbon assimilation for natural vegetation
!!! based on RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
        
  implicit none
        
contains

  subroutine CarbonFluxNatureVeg(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CO2FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
        
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: SC          ! temperature stress death coefficient
    real(kind=kind_noahmp)           :: SD          ! water stress death coefficient
    ! Respiration as a function of temperature
    real(kind=kind_noahmp)           :: r,x
    r(x) = exp(0.08 * (x - 298.16))

!------------------------------------------------------------------------
    associate(                                                        &
              VEGTYP          => noahmp%config%domain%VEGTYP         ,& ! in,    dynamic vegetation option
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              EBLFOREST       => noahmp%config%domain%EBLFOREST      ,& ! in,    flag for Evergreen Broadleaf Forest
              WRRAT           => noahmp%biochem%param%WRRAT          ,& ! in,    wood to non-wood ratio
              LTOVRC          => noahmp%biochem%param%LTOVRC         ,& ! in,    leaf turnover coefficient [1/s]
              TDLEF           => noahmp%biochem%param%TDLEF          ,& ! in,    characteristic T for leaf freezing [K]
              DILEFW          => noahmp%biochem%param%DILEFW         ,& ! in,    coeficient for leaf water stress death [1/s]
              DILEFC          => noahmp%biochem%param%DILEFC         ,& ! in,    coeficient for leaf temperature stress death [1/s]
              FRAGR           => noahmp%biochem%param%FRAGR          ,& ! in,    fraction of growth respiration  !original was 0.3
              TMIN            => noahmp%biochem%param%TMIN           ,& ! in,    minimum temperature for photosynthesis (k)
              MRP             => noahmp%biochem%param%MRP            ,& ! in,    microbial respiration parameter (umol co2/kg c/s)
              FOLNMX          => noahmp%biochem%param%FOLNMX         ,& ! in,    foliage nitrogen concentration when f(n)=1 (%)
              ARM             => noahmp%biochem%param%ARM            ,& ! in,    q10 for maintenance respiration
              RMF25           => noahmp%biochem%param%RMF25          ,& ! in,    leaf maintenance respiration at 25c (umol co2/m**2/s)
              RMR25           => noahmp%biochem%param%RMR25          ,& ! in,    root maintenance respiration at 25c (umol co2/kg bio/s)
              RMS25           => noahmp%biochem%param%RMS25          ,& ! in,    stem maintenance respiration at 25c (umol co2/kg bio/s)
              WDPOOL          => noahmp%biochem%param%WDPOOL         ,& ! in,    wood pool (switch 1 or 0) depending on woody or not
              RTOVRC          => noahmp%biochem%param%RTOVRC         ,& ! in,    root turnover coefficient [1/s]
              RSDRYC          => noahmp%biochem%param%RSDRYC         ,& ! in,    degree of drying that reduces soil respiration [-]
              RSWOODC         => noahmp%biochem%param%RSWOODC        ,& ! in,    wood respiration coeficient [1/s]
              BF              => noahmp%biochem%param%BF             ,& ! in,    parameter for present wood allocation [-]
              WSTRC           => noahmp%biochem%param%WSTRC          ,& ! in,    water stress coeficient [-]
              LAIMIN          => noahmp%biochem%param%LAIMIN         ,& ! in,    minimum leaf area index [m2/m2]
              XSAMIN          => noahmp%biochem%param%XSAMIN         ,& ! in,    minimum stem area index [m2/m2]
              IGS             => noahmp%biochem%state%IGS            ,& ! in,    growing season index (0=off, 1=on)
              FOLN            => noahmp%biochem%state%FOLN           ,& ! in,    foliage nitrogen concentration (%)
              LAPM            => noahmp%biochem%state%LAPM           ,& ! in,    leaf area per unit mass [m2/g]
              PSN             => noahmp%biochem%flux%PSN             ,& ! in,    total leaf photosynthesis (umol co2 /m2 /s)
              WROOT           => noahmp%water%state%WROOT            ,& ! in,    root zone soil water [-]
              WSTRES          => noahmp%water%state%WSTRES           ,& ! in,    water stress coeficient [-]  (1. for wilting)
              STC             => noahmp%energy%state%STC             ,& ! in,    snow and soil layer temperature [K]
              TROOT           => noahmp%energy%state%TROOT           ,& ! in,    root-zone averaged temperature (k)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              XLAI            => noahmp%energy%state%LAI             ,& ! inout, leaf area index [-]
              XSAI            => noahmp%energy%state%SAI             ,& ! inout, stem area index [-]
              LFMASS          => noahmp%biochem%state%LFMASS         ,& ! inout, leaf mass [g/m2]
              RTMASS          => noahmp%biochem%state%RTMASS         ,& ! inout, mass of fine roots [g/m2]
              STMASS          => noahmp%biochem%state%STMASS         ,& ! inout, stem mass [g/m2]
              WOOD            => noahmp%biochem%state%WOOD           ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              STBLCP          => noahmp%biochem%state%STBLCP         ,& ! inout, stable carbon in deep soil [g/m2]
              FASTCP          => noahmp%biochem%state%FASTCP         ,& ! inout, short-lived carbon in shallow soil [g/m2]
              TOTSC           => noahmp%biochem%state%TOTSC          ,& ! out,   total soil carbon [g/m2 C]
              TOTLB           => noahmp%biochem%state%TOTLB          ,& ! out,   total living carbon ([g/m2 C]
              LFMSMN          => noahmp%biochem%state%LFMSMN         ,& ! out,   minimum leaf mass [g/m2]
              ADDNPPLF        => noahmp%biochem%state%ADDNPPLF       ,& ! out,   leaf assimil after resp. losses removed [g/m2]
              ADDNPPST        => noahmp%biochem%state%ADDNPPST       ,& ! out,   stem assimil after resp. losses removed [g/m2]
              LEAFPT          => noahmp%biochem%state%LEAFPT         ,& ! out,   fraction of carbon allocated to leaves [-]
              WOODF           => noahmp%biochem%state%WOODF          ,& ! out,   calculated wood to root ratio [-]
              NONLEF          => noahmp%biochem%state%NONLEF         ,& ! out,   fraction of carbon to root and wood [-]
              ROOTPT          => noahmp%biochem%state%ROOTPT         ,& ! out,   fraction of carbon flux to roots [-]
              WOODPT          => noahmp%biochem%state%WOODPT         ,& ! out,   fraction of carbon flux to wood [-]
              STEMPT          => noahmp%biochem%state%STEMPT         ,& ! out,   fraction of carbon flux to stem [-]
              FSW             => noahmp%biochem%state%FSW            ,& ! out,   soil water factor for microbial respiration
              FST             => noahmp%biochem%state%FST            ,& ! out,   soil temperature factor for microbial respiration
              FNF             => noahmp%biochem%state%FNF            ,& ! out,   foliage nitrogen adjustemt to respiration (<= 1)
              TF              => noahmp%biochem%state%TF             ,& ! out,   temperature factor
              RF              => noahmp%biochem%state%RF             ,& ! out,   respiration reduction factor (<= 1)
              STMSMN          => noahmp%biochem%state%STMSMN         ,& ! out,   minimum stem mass [g/m2]
              SAPM            => noahmp%biochem%state%SAPM           ,& ! out,   stem area per unit mass [m2/g]
              CARBFX          => noahmp%biochem%flux%CARBFX          ,& ! out,   carbon assimilated rate [gC/m2/s]
              GPP             => noahmp%biochem%flux%GPP             ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NPP             => noahmp%biochem%flux%NPP             ,& ! out,   net primary productivity [g/m2/s C]
              NEE             => noahmp%biochem%flux%NEE             ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              AUTORS          => noahmp%biochem%flux%AUTORS          ,& ! out,   net ecosystem respiration [g/m2/s C]
              HETERS          => noahmp%biochem%flux%HETERS          ,& ! out,   organic respiration [g/m2/s C]
              CFLUX           => noahmp%biochem%flux%CFLUX           ,& ! out,   carbon flux to atmosphere [g/m2/s]
              NPPL            => noahmp%biochem%flux%NPPL            ,& ! out,   leaf net primary productivity [g/m2/s]
              NPPR            => noahmp%biochem%flux%NPPR            ,& ! out,   root net primary productivity [g/m2/s]
              NPPW            => noahmp%biochem%flux%NPPW            ,& ! out,   wood net primary productivity [g/m2/s]
              NPPS            => noahmp%biochem%flux%NPPS            ,& ! out,   stem net primary productivity [g/m2/s]
              GRLEAF          => noahmp%biochem%flux%GRLEAF          ,& ! out,   growth respiration rate for leaf [g/m2/s]
              GRROOT          => noahmp%biochem%flux%GRROOT          ,& ! out,   growth respiration rate for root [g/m2/s]
              GRWOOD          => noahmp%biochem%flux%GRWOOD          ,& ! out,   growth respiration rate for wood [g/m2/s]
              GRSTEM          => noahmp%biochem%flux%GRSTEM          ,& ! out,   growth respiration rate for stem [g/m2/s]
              LFDEL           => noahmp%biochem%flux%LFDEL           ,& ! out,   maximum leaf mass available to change [g/m2/s]
              STABLC          => noahmp%biochem%flux%STABLC          ,& ! out,   decay rate of fast carbon to slow carbon [g/m2/s]
              RESP            => noahmp%biochem%flux%RESP            ,& ! out,   leaf respiration [umol/m2/s]
              RSSTEM          => noahmp%biochem%flux%RSSTEM          ,& ! out,   stem respiration [g/m2/s]
              RSWOOD          => noahmp%biochem%flux%RSWOOD          ,& ! out,   wood respiration [g/m2/s]
              RSLEAF          => noahmp%biochem%flux%RSLEAF          ,& ! out,   leaf maintenance respiration [g/m2/s]
              RSROOT          => noahmp%biochem%flux%RSROOT          ,& ! out,   fine root respiration [g/m2/s]
              RSSOIL          => noahmp%biochem%flux%RSSOIL          ,& ! out,   soil respiration [g/m2/s]
              DIELF           => noahmp%biochem%flux%DIELF           ,& ! out,   death rate of leaf mass [g/m2/s]
              DIEST           => noahmp%biochem%flux%DIEST           ,& ! out,   death rate of stem mass [g/m2/s]
              LFTOVR          => noahmp%biochem%flux%LFTOVR          ,& ! out,   stem turnover rate [g/m2/s]
              STTOVR          => noahmp%biochem%flux%STTOVR          ,& ! out,   stem turnover rate [g/m2/s]
              WDTOVR          => noahmp%biochem%flux%WDTOVR          ,& ! out,   wood turnover rate [g/m2/s]
              RTTOVR          => noahmp%biochem%flux%RTTOVR          ,& ! out,   root carbon loss rate by turnover [g/m2/s]
              STDEL           => noahmp%biochem%flux%STDEL            & ! out,   maximum steam mass available to change [g/m2/s]
             )
!-----------------------------------------------------------------------

    ! initialization
    SAPM    = 3.0 * 0.001      ! m2/kg -->m2/g
    LFMSMN  = LAIMIN / LAPM
    STMSMN  = XSAMIN / SAPM
        
    ! respiration
    if ( IGS == 0.0 ) then
       RF = 0.5
    else
       RF = 1.0
    endif             
    FNF    = min( FOLN / max(1.0e-06,FOLNMX), 1.0 )
    TF     = ARM**((TV - 298.16) / 10.0)
    RESP   = RMF25 * TF * FNF * XLAI * RF * (1.0 - WSTRES)           ! umol/m2/s
    RSLEAF = min( (LFMASS-LFMSMN)/DT, RESP*12.0e-6 )                 ! g/m2/s
    RSROOT = RMR25 * (RTMASS*1.0e-3) * TF * RF * 12.0e-6             ! g/m2/s
    RSSTEM = RMS25 * ((STMASS-STMSMN) * 1.0e-3) * TF * RF * 12.0e-6  ! g/m2/s
    RSWOOD = RSWOODC * r(TV) * WOOD * WDPOOL
    
    !!! carbon assimilation start
    ! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;   
    CARBFX  = PSN * 12.0e-6      ! umol co2 /m2/ s -> g/m2/s carbon

    ! fraction of carbon into leaf versus nonleaf
    LEAFPT = exp(0.01 * (1.0 - exp(0.75*XLAI)) * XLAI)
    if ( VEGTYP == EBLFOREST ) LEAFPT = exp(0.01 * (1.0 - exp(0.50*XLAI)) * XLAI)
    NONLEF = 1.0 - LEAFPT
    STEMPT = XLAI / 10.0 * LEAFPT
    LEAFPT = LEAFPT - STEMPT
      
    !  fraction of carbon into wood versus root 
    if ( WOOD > 1.0e-6 ) then
       WOODF = (1.0 - exp(-BF * (WRRAT*RTMASS/WOOD)) / BF) * WDPOOL
    else
       WOODF = WDPOOL
    endif   
    ROOTPT = NONLEF * (1.0 - WOODF)
    WOODPT = NONLEF * WOODF

    ! leaf and root turnover per time step  
    LFTOVR = LTOVRC * 5.0e-7 * LFMASS
    STTOVR = LTOVRC * 5.0e-7 * STMASS
    RTTOVR = RTOVRC * RTMASS
    WDTOVR = 9.5e-10 * WOOD
       
    ! seasonal leaf die rate dependent on temp and water stress
    ! water stress is set to 1 at permanent wilting point      
    SC    = exp(-0.3 * max(0.0, TV-TDLEF)) * (LFMASS / 120.0) 
    SD    = exp((WSTRES - 1.0) * WSTRC)
    DIELF = LFMASS * 1.0e-6 * (DILEFW * SD + DILEFC * SC)
    DIEST = STMASS * 1.0e-6 * (DILEFW * SD + DILEFC * SC)
     
    ! calculate growth respiration for leaf, rtmass and wood 
    GRLEAF = max( 0.0, FRAGR * (LEAFPT*CARBFX - RSLEAF) )
    GRSTEM = max( 0.0, FRAGR * (STEMPT*CARBFX - RSSTEM) )
    GRROOT = max( 0.0, FRAGR * (ROOTPT*CARBFX - RSROOT) )
    GRWOOD = max( 0.0, FRAGR * (WOODPT*CARBFX - RSWOOD) )
        
    ! Impose lower T limit for photosynthesis
    ADDNPPLF = max( 0.0, LEAFPT*CARBFX - GRLEAF - RSLEAF )
    ADDNPPST = max( 0.0, STEMPT*CARBFX - GRSTEM - RSSTEM )
    !ADDNPPLF = LEAFPT*CARBFX - GRLEAF - RSLEAF  ! MB: test Kjetil 
    !ADDNPPST = STEMPT*CARBFX - GRSTEM - RSSTEM  ! MB: test Kjetil 
    if ( TV < TMIN ) ADDNPPLF = 0.0
    if ( TV < TMIN ) ADDNPPST = 0.0
     
    ! update leaf, root, and wood carbon
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LFDEL = (LFMASS - LFMSMN) / DT
    STDEL = (STMASS - STMSMN) / DT
    DIELF = min( DIELF, LFDEL+ADDNPPLF-LFTOVR )
    DIEST = min( DIEST, STDEL+ADDNPPST-STTOVR )
      
    ! net primary productivities
    NPPL  = max( ADDNPPLF, -LFDEL )
    NPPS  = max( ADDNPPST, -STDEL )
    NPPR  = ROOTPT * CARBFX - RSROOT - GRROOT
    NPPW  = WOODPT * CARBFX - RSWOOD - GRWOOD
       
    ! masses of plant components
    LFMASS = LFMASS + (NPPL - LFTOVR - DIELF) * DT
    STMASS = STMASS + (NPPS - STTOVR - DIEST) * DT   ! g/m2
    RTMASS = RTMASS + (NPPR - RTTOVR) * DT
    if ( RTMASS < 0.0 ) then
       RTTOVR = NPPR
       RTMASS = 0.0
    endif 
    WOOD = (WOOD + (NPPW - WDTOVR) * DT ) * WDPOOL

    ! soil carbon budgets 
    FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF+DIEST) * DT  ! MB: add DIEST v3.7
    FST    = 2.0**( (STC(1) - 283.16) / 10.0 )
    FSW    = WROOT / (0.20 + WROOT) * 0.23 / (0.23 + WROOT)
    RSSOIL = FSW * FST * MRP * max(0.0, FASTCP*1.0e-3) * 12.0e-6
    STABLC = 0.1 * RSSOIL
    FASTCP = FASTCP - (RSSOIL + STABLC) * DT
    STBLCP = STBLCP + STABLC * DT
     
    !  total carbon flux     
    CFLUX  = - CARBFX + RSLEAF + RSROOT + RSWOOD + RSSTEM &     ! MB: add RSSTEM,GRSTEM,0.9*RSSOIL v3.7
             + 0.9*RSSOIL + GRLEAF + GRROOT + GRWOOD + GRSTEM   ! g/m2/s

    ! for outputs   
    GPP    = CARBFX                                             !g/m2/s C
    NPP    = NPPL + NPPW + NPPR +NPPS                           !g/m2/s C
    AUTORS = RSROOT + RSWOOD + RSLEAF + RSSTEM + &              !g/m2/s C  MB: add RSSTEM, GRSTEM v3.7
             GRLEAF + GRROOT + GRWOOD + GRSTEM                  !g/m2/s C  MB: add 0.9* v3.7
    HETERS = 0.9 * RSSOIL                                       !g/m2/s C
    NEE    = (AUTORS + HETERS - GPP) * 44.0 / 12.0              !g/m2/s CO2
    TOTSC  = FASTCP + STBLCP                                    !g/m2   C
    TOTLB  = LFMASS + RTMASS + STMASS + WOOD                    !g/m2   C  MB: add STMASS v3.7
    
    ! leaf area index and stem area index
    XLAI   = max( LFMASS*LAPM, LAIMIN )
    XSAI   = max( STMASS*SAPM, XSAMIN )

    end associate

  end subroutine CarbonFluxNatureVeg

end module CarbonFluxNatureVegMod
