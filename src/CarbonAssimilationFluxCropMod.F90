module CarbonAssimilationFluxCropMod

    !!! Main Carbon Assimilation for Crop
        
    use Machine, only : kind_noahmp
    use NoahmpVarType
        
    implicit none
        
contains

    subroutine CarbonAssimilationFluxCrop (noahmp)

        ! ------------------------ Code history -----------------------------------
        ! Original Noah-MP subroutine: CO2FLUX_CROP
        ! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
        ! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
        ! -------------------------------------------------------------------------
        
        implicit none
        
        type(noahmp_type), intent(inout) :: noahmp

        associate(
                 ADDNPPLF =>  noahmp%biochem%state%ADDNPPLF   ,&
                 ADDNPPST =>  noahmp%biochem%state%ADDNPPST   ,&
                 AUTORS   =>  noahmp%biochem%flux%AUTORS      ,&
                 CARBFX   =>  noahmp%biochem%state%CARBFX     ,&
                 CBHYDRAFX=>  noahmp%biochem%state%CBHYDRAFX  ,&
                 CFLUX    =>  noahmp%biochem%flux%CFLUX       ,&
                 DIELF    =>  noahmp%biochem%state%DIELF      ,&
                 DIEST    =>  noahmp%biochem%state%DIEST      ,&
                 DT       =>  noahmp%config%domain%DT         ,&
                 FASTCP   =>  noahmp%biochem%state%FASTCP     ,&
                 FNF      =>  noahmp%biochem%state%FNF        ,&
                 FOLN     =>  noahmp%biochem%state%FOLN       ,&
                 FST      =>  noahmp%biochem%state%FST        ,&
                 FSW      =>  noahmp%biochem%state%FSW        ,&
                 GDD      =>  noahmp%biochem%state%GDD        ,&
                 GPP      =>  noahmp%biochem%flux%GPP         ,&
                 GRAIN    =>  noahmp%biochem%state%GRAIN      ,&
                 GRGRAIN  =>  noahmp%biochem%flux%GRGRAIN     ,&
                 GRLEAF   =>  noahmp%biochem%flux%GRLEAF      ,&
                 GRROOT   =>  noahmp%biochem%flux%GRROOT      ,&
                 GRSTEM   =>  noahmp%biochem%flux%GRSTEM      ,&
                 GRTOVR   =>  noahmp%biochem%state%GRTOVR     ,&
                 GRWOOD   =>  noahmp%biochem%flux%GRWOOD      ,&
                 HETERS   =>  noahmp%biochem%flux%HETERS      ,&
                 IHA      =>  noahmp%biochem%state%IHA        ,&
                 IPA      =>  noahmp%biochem%state%IPA        ,&       
                 LEAFPT   =>  noahmp%biochem%state%LEAFPT     ,&
                 LFCONVERT=>  noahmp%biochem%state%LFCONVERT  ,&
                 LFDEL    =>  noahmp%biochem%flux%LFDEL       ,&
                 LFMASS   =>  noahmp%biochem%state%LFMASS     ,&  
                 LFMSMN   =>  noahmp%biochem%state%LFMSMN     ,&
                 LFTOVR   =>  noahmp%biochem%state%LFTOVR     ,&
                 NEE      =>  noahmp%biochem%flux%NEE         ,&
                 NONLEF   =>  noahmp%biochem%state%NONLEF     ,&
                 NPP      =>  noahmp%biochem%flux%NPP         ,&
                 NPPG     =>  noahmp%biochem%flux%NPPG        ,&
                 NPPL     =>  noahmp%biochem%flux%NPPL        ,&
                 NPPR     =>  noahmp%biochem%flux%NPPR        ,&
                 NPPS     =>  noahmp%biochem%flux%NPPS        ,&
                 NPPW     =>  noahmp%biochem%flux%NPPW        ,&
                 PGS      =>  noahmp%biochem%state%PGS        ,&
                 PSN      =>  noahmp%biochem%flux%PSN         ,&
                 RESP     =>  noahmp%biochem%flux%RESP        ,&
                 RSGRAIN  =>  noahmp%biochem%state%RSGRAIN    ,&
                 RSLEAF   =>  noahmp%biochem%state%RSLEAF     ,&
                 RSROOT   =>  noahmp%biochem%state%RSROOT     ,&
                 RSSOIL   =>  noahmp%biochem%state%RSSOIL     ,&
                 RSSTEM   =>  noahmp%biochem%flux%RSSTEM      ,&
                 RSWOOD   =>  noahmp%biochem%state%RSWOOD     ,&
                 RTCONVERT=>  noahmp%biochem%flux%RTCONVERT   ,&
                 RTMASS   =>  noahmp%biochem%state%RTMASS     ,&
                 RTTOVR   =>  noahmp%biochem%state%RTTOVR     ,&
                 SAPM     =>  noahmp%biochem%state%SAPM       ,&
                 STABLC   =>  noahmp%biochem%flux%STABLC      ,&
                 STBLCP   =>  noahmp%biochem%state%STBLCP     ,&
                 STC      =>  noahmp%energy%state%STC         ,&
                 STCONVERT=>  noahmp%biochem%flux%STCONVERT   ,&
                 STDEL    =>  noahmp%biochem%flux%STDEL       ,&
                 STMASS   =>  noahmp%biochem%state%STMASS     ,&
                 STMSMN   =>  noahmp%biochem%state%STMSMN     ,&
                 STTOVR   =>  noahmp%biochem%state%STTOVR     ,&
                 TF       =>  noahmp%biochem%state%TF         ,&
                 TOTLB    =>  noahmp%biochem%state%TOTLB      ,&
                 TOTSC    =>  noahmp%biochem%state%TOTSC      ,&
                 TV       =>  noahmp%energy%state%TV          ,&
                 WDTOVR   =>  noahmp%biochem%state%WDTOVR     ,&
                 WOOD     =>  noahmp%biochem%state%WOOD       ,&
                 WOODF    =>  noahmp%biochem%state%WOODF      ,&
                 WROOT    =>  noahmp%water%state%WROOT        ,&
                 WSTRES   =>  noahmp%water%state%WSTRES       ,&
                 XLAI     =>  noahmp%energy%state%LAI         ,&
                 XSAI     =>  noahmp%energy%state%SAI         ,&
                 EBLFOREST=>  noahmp%config%domain%EBLFOREST  ,& 
                 FOLN_MX  =>  noahmp%biochem%param%FOLN_MX    ,& 
                 Q10MR    =>  noahmp%biochem%param%Q10MR      ,& 
                 LFMR25   =>  noahmp%biochem%param%LFMR25     ,& 
                 RTMR25   =>  noahmp%biochem%param%RTMR25     ,& 
                 STMR25   =>  noahmp%biochem%param%STMR25     ,& 
                 GRAINMR25=>  noahmp%biochem%param%GRAINMR25  ,& 
                 FRA_GR   =>  noahmp%biochem%param%FRA_GR     ,& 
                 LFPT     =>  noahmp%biochem%param%LFPT       ,& 
                 STPT     =>  noahmp%biochem%param%STPT       ,& 
                 RTPT     =>  noahmp%biochem%param%RTPT       ,& 
                 GRAINPT  =>  noahmp%biochem%param%GRAINPT    ,& 
                 LF_OVRC  =>  noahmp%biochem%param%LF_OVRC    ,& 
                 RT_OVRC  =>  noahmp%biochem%param%RT_OVRC    ,& 
                 ST_OVRC  =>  noahmp%biochem%param%ST_OVRC    ,& 
                 LEFREEZ  =>  noahmp%biochem%param%LEFREEZ    ,& 
                 DILE_FW  =>  noahmp%biochem%param%DILE_FW    ,& 
                 DILE_FC  =>  noahmp%biochem%param%DILE_FC    ,& 
                 LFCT     =>  noahmp%biochem%param%LFCT       ,& 
                 STCT     =>  noahmp%biochem%param%STCT       ,& 
                 RTCT     =>  noahmp%biochem%param%RTCT       ,& 
                 MRP      =>  noahmp%biochem%param%MRP        ,& 
                 BIO2LAI  =>  noahmp%biochem%param%BIO2LAI     & 
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
        real(kind=kind_noahmp)           :: r,x

        ! ---------------------------------------------------------------------------------

        ! Respiration as a function of temperature

        r(x) = exp(0.08*(x-298.16))

        ! constants

        RSDRYC  = 40.0              ! original was 40.0
        RSWOODC = 3.0E-10           
        BF      = 0.90              ! original was 0.90   ! carbon to roots
        WSTRC   = 100.0
        LAIMIN  = 0.05
        XSAMIN  = 0.05
        SAPM    = 3.0*0.001         ! m2/kg -->m2/g
        LFMSMN  = laimin/0.035
        STMSMN  = xsamin/sapm

        ! ---------------------------------------------------------------------------------

        ! carbon assimilation
        ! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20

        CARBFX     = PSN*12.0e-6      !*IPA   !umol co2 /m2/ s -> g/m2/s C
        CBHYDRAFX  = PSN*30.0e-6      !*IPA

        ! mainteinance respiration

        FNF     = MIN( FOLN/MAX(1.0E-06,FOLN_MX), 1.0 )
        TF      = Q10MR**( (TV-298.16)/10.0 )
        RESP    = LFMR25 * TF * FNF * XLAI  * (1.-WSTRES)         ! umol/m2/s
        RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*30.0e-6)            ! g/m2/s
        RSROOT  = RTMR25*(RTMASS*1E-3)*TF * 30.0e-6               ! g/m2/s
        RSSTEM  = STMR25*(STMASS*1E-3)*TF * 30.0e-6               ! g/m2/s
        RSGRAIN = GRAINMR25*(GRAIN*1E-3)*TF * 30.0e-6             ! g/m2/s

        ! calculate growth respiration for leaf, rtmass and grain

        GRLEAF  = MAX(0.0,FRA_GR*(LFPT(PGS)*CBHYDRAFX  - RSLEAF))
        GRSTEM  = MAX(0.0,FRA_GR*(STPT(PGS)*CBHYDRAFX  - RSSTEM))
        GRROOT  = MAX(0.0,FRA_GR*(RTPT(PGS)*CBHYDRAFX  - RSROOT))
        GRGRAIN = MAX(0.0,FRA_GR*(GRAINPT(PGS)*CBHYDRAFX  - RSGRAIN))

        ! leaf turnover, stem turnover, root turnover and leaf death caused by soil
        ! water and soil temperature stress

        LFTOVR  = LF_OVRC(PGS)*1.0E-6*LFMASS
        RTTOVR  = RT_OVRC(PGS)*1.0E-6*RTMASS
        STTOVR  = ST_OVRC(PGS)*1.0E-6*STMASS
        SC  = EXP(-0.3*MAX(0.0,TV-LEFREEZ)) * (LFMASS/120.0)
        SD  = EXP((WSTRES-1.0)*WSTRC)
        DIELF = LFMASS*1.0E-6*(DILE_FW(PGS) * SD + DILE_FC(PGS)*SC)

        ! Allocation of CBHYDRAFX to leaf, stem, root and grain at each growth stage

        ADDNPPLF    = MAX(0.0,LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF)
        ADDNPPLF    = LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF
        ADDNPPST    = MAX(0.0,STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM)
        ADDNPPST    = STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM
    
        ! avoid reducing leaf mass below its minimum value but conserve mass

        LFDEL = (LFMASS - LFMSMN)/DT
        STDEL = (STMASS - STMSMN)/DT
        LFTOVR  = MIN(LFTOVR,LFDEL+ADDNPPLF)
        STTOVR  = MIN(STTOVR,STDEL+ADDNPPST)
        DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)

        ! net primary productivities

        NPPL   = MAX(ADDNPPLF,-LFDEL)
        NPPL   = ADDNPPLF
        NPPS   = MAX(ADDNPPST,-STDEL)
        NPPS   = ADDNPPST
        NPPR   = RTPT(PGS)*CBHYDRAFX - RSROOT - GRROOT
        NPPG  =  GRAINPT(PGS)*CBHYDRAFX - RSGRAIN - GRGRAIN

        ! masses of plant components
  
        LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
        STMASS = STMASS + (NPPS-STTOVR)*DT                    ! g/m2
        RTMASS = RTMASS + (NPPR-RTTOVR)*DT
        GRAIN =  GRAIN + NPPG*DT 
        GPP = CBHYDRAFX* 0.4                                  ! g/m2/s C  0.4=12/30, CH20 to C
        LFCONVERT = 0.0                                       ! Zhe Zhang 2020-07-13
        STCONVERT = 0.0
        RTCONVERT = 0.0
        LFCONVERT = LFMASS*(LFCT(PGS)*DT/3600.0)
        STCONVERT = STMASS*(STCT(PGS)*DT/3600.0)
        RTCONVERT = RTMASS*(RTCT(PGS)*DT/3600.0)
        LFMASS = LFMASS - LFCONVERT
        STMASS = STMASS - STCONVERT
        RTMASS = RTMASS - RTCONVERT
        GRAIN  = GRAIN + STCONVERT + RTCONVERT + LFCONVERT

        !IF(PGS==6) THEN
        !  STCONVERT = STMASS*(0.00005*DT/3600.0)
        !  STMASS = STMASS - STCONVERT
        !  RTCONVERT = RTMASS*(0.0005*DT/3600.0)
        !  RTMASS = RTMASS - RTCONVERT
        !  GRAIN  = GRAIN + STCONVERT + RTCONVERT
        !END IF
    
        if (RTMASS.lt.0.0) then
           RTTOVR = NPPR
           RTMASS = 0.0
        endif
        if (GRAIN.lt.0.0) then
           GRAIN = 0.0
        endif

        ! soil carbon budgets
        !     IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
        !       FASTCP=1000
        !     ELSE
        FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+DIELF)*DT 
        !     END IF

        FST = 2.0**( (STC-283.16)/10.0 )
        FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
        RSSOIL = FSW * FST * MRP* MAX(0.0,FASTCP*1.0E-3)*12.0E-6
        STABLC = 0.1*RSSOIL
        FASTCP = FASTCP - (RSSOIL + STABLC)*DT
        STBLCP = STBLCP + STABLC*DT
 
        !  total carbon flux
 
        CFLUX  = - CARBFX + RSLEAF + RSROOT  + RSSTEM &
                 + RSSOIL + GRLEAF + GRROOT                           !g/m2/s 0.4=12/30, CH20 to C
        ! for outputs
                                                                      !g/m2/s C
        NPP   = (NPPL + NPPS+ NPPR +NPPG)*0.4                         !g/m2/s C  0.4=12/30, CH20 to C
 
  
        AUTORS = RSROOT + RSGRAIN  + RSLEAF +  &                      !g/m2/s C
                 GRLEAF + GRROOT + GRGRAIN                            !g/m2/s C
        HETERS = RSSOIL                                               !g/m2/s C
        NEE    = (AUTORS + HETERS - GPP)*44.0/30.0                    !g/m2/s CO2
        TOTSC  = FASTCP + STBLCP                                      !g/m2   C
        TOTLB  = LFMASS + RTMASS + GRAIN         
 
        ! leaf area index and stem area index
  
        XLAI    = MAX(LFMASS*BIO2LAI,LAIMIN)
        XSAI    = MAX(STMASS*SAPM,XSAMIN)
   
        !After harversting
        !     IF(PGS == 8 ) THEN
        !       LFMASS = 0.62
        !       STMASS = 0
        !       GRAIN  = 0
        !     END IF

        !    IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
        if (PGS == 8 .and. (GRAIN > 0.0 .or. LFMASS > 0 .or. STMASS > 0 .or. RTMASS > 0)) then
           XLAI   = 0.05
           XSAI   = 0.05
           LFMASS = LFMSMN
           STMASS = STMSMN
           RTMASS = 0
           GRAIN  = 0
        endif 
        
        end associate

    end subroutine CarbonAssimilationFluxCrop

end module CarbonAssimilationFluxCropMod