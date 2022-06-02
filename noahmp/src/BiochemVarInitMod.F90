module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine BiochemVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate( NSTAGE => noahmp%config%domain%NSTAGE )

    ! biochem state variables
    noahmp%biochem%state%PGS            = huge(1  )
    noahmp%biochem%state%IPA            = huge(1  )
    noahmp%biochem%state%IHA            = huge(1  )
    noahmp%biochem%state%IGS            = huge(1.0)
    noahmp%biochem%state%FOLN           = huge(1.0)
    noahmp%biochem%state%LFMASS         = huge(1.0)
    noahmp%biochem%state%RTMASS         = huge(1.0)
    noahmp%biochem%state%STMASS         = huge(1.0)
    noahmp%biochem%state%WOOD           = huge(1.0)
    noahmp%biochem%state%STBLCP         = huge(1.0)
    noahmp%biochem%state%FASTCP         = huge(1.0)
    noahmp%biochem%state%TOTSC          = huge(1.0)
    noahmp%biochem%state%TOTLB          = huge(1.0)
    noahmp%biochem%state%LAPM           = huge(1.0)
    noahmp%biochem%state%SAPM           = huge(1.0)
    noahmp%biochem%state%LFMSMN         = huge(1.0)
    noahmp%biochem%state%STMSMN         = huge(1.0)
    noahmp%biochem%state%ADDNPPLF       = huge(1.0)
    noahmp%biochem%state%ADDNPPST       = huge(1.0)
    noahmp%biochem%state%LEAFPT         = huge(1.0)
    noahmp%biochem%state%WOODF          = huge(1.0)
    noahmp%biochem%state%NONLEF         = huge(1.0)
    noahmp%biochem%state%ROOTPT         = huge(1.0)
    noahmp%biochem%state%WOODPT         = huge(1.0)
    noahmp%biochem%state%STEMPT         = huge(1.0)
    noahmp%biochem%state%FSW            = huge(1.0)
    noahmp%biochem%state%FST            = huge(1.0)
    noahmp%biochem%state%FNF            = huge(1.0)
    noahmp%biochem%state%TF             = huge(1.0)
    noahmp%biochem%state%RF             = huge(1.0)
    noahmp%biochem%state%GRAIN          = huge(1.0)
    noahmp%biochem%state%GDD            = huge(1.0)

    ! biochem flux variables
    noahmp%biochem%flux%PSNSUN          = huge(1.0)
    noahmp%biochem%flux%PSNSHA          = huge(1.0)
    noahmp%biochem%flux%PSN             = huge(1.0)
    noahmp%biochem%flux%GPP             = huge(1.0)
    noahmp%biochem%flux%NPP             = huge(1.0)
    noahmp%biochem%flux%NEE             = huge(1.0)
    noahmp%biochem%flux%AUTORS          = huge(1.0)
    noahmp%biochem%flux%HETERS          = huge(1.0)
    noahmp%biochem%flux%CFLUX           = huge(1.0)
    noahmp%biochem%flux%NPPL            = huge(1.0)
    noahmp%biochem%flux%NPPR            = huge(1.0)
    noahmp%biochem%flux%NPPW            = huge(1.0)
    noahmp%biochem%flux%NPPS            = huge(1.0)
    noahmp%biochem%flux%GRLEAF          = huge(1.0)
    noahmp%biochem%flux%GRROOT          = huge(1.0)
    noahmp%biochem%flux%GRWOOD          = huge(1.0)
    noahmp%biochem%flux%GRSTEM          = huge(1.0)
    noahmp%biochem%flux%LFDEL           = huge(1.0)
    noahmp%biochem%flux%STDEL           = huge(1.0)
    noahmp%biochem%flux%STABLC          = huge(1.0)
    noahmp%biochem%flux%RESP            = huge(1.0)
    noahmp%biochem%flux%RSSTEM          = huge(1.0)
    noahmp%biochem%flux%GRGRAIN         = huge(1.0)
    noahmp%biochem%flux%NPPG            = huge(1.0)
    noahmp%biochem%flux%RTCONVERT       = huge(1.0)
    noahmp%biochem%flux%STCONVERT       = huge(1.0)
    noahmp%biochem%flux%PSNCROP         = huge(1.0)
    noahmp%biochem%flux%RSWOOD          = huge(1.0)
    noahmp%biochem%flux%RSLEAF          = huge(1.0)
    noahmp%biochem%flux%RSROOT          = huge(1.0)
    noahmp%biochem%flux%DIELF           = huge(1.0)
    noahmp%biochem%flux%DIEST           = huge(1.0)
    noahmp%biochem%flux%CARBFX          = huge(1.0)
    noahmp%biochem%flux%LFTOVR          = huge(1.0)
    noahmp%biochem%flux%STTOVR          = huge(1.0)
    noahmp%biochem%flux%WDTOVR          = huge(1.0)
    noahmp%biochem%flux%RSSOIL          = huge(1.0)
    noahmp%biochem%flux%RTTOVR          = huge(1.0)
    noahmp%biochem%flux%CBHYDRAFX       = huge(1.0)
    noahmp%biochem%flux%GRTOVR          = huge(1.0)
    noahmp%biochem%flux%LFCONVERT       = huge(1.0)
    noahmp%biochem%flux%RSGRAIN         = huge(1.0)
 
    ! biochem parameter variables
    noahmp%biochem%param%DEFAULT_CROP   = huge(1  )
    noahmp%biochem%param%PLTDAY         = huge(1  )
    noahmp%biochem%param%HSDAY          = huge(1  )
    noahmp%biochem%param%C3C4           = huge(1  )
    noahmp%biochem%param%FOLNMX         = huge(1.0)
    noahmp%biochem%param%QE25           = huge(1.0)
    noahmp%biochem%param%VCMX25         = huge(1.0)
    noahmp%biochem%param%AVCMX          = huge(1.0)
    noahmp%biochem%param%C3PSN          = huge(1.0)
    noahmp%biochem%param%MP             = huge(1.0)
    noahmp%biochem%param%TMIN           = huge(1.0)
    noahmp%biochem%param%SLA            = huge(1.0)
    noahmp%biochem%param%FOLN_MX        = huge(1.0)
    noahmp%biochem%param%ARM            = huge(1.0)
    noahmp%biochem%param%RMF25          = huge(1.0)
    noahmp%biochem%param%RMS25          = huge(1.0)
    noahmp%biochem%param%RMR25          = huge(1.0)
    noahmp%biochem%param%WRRAT          = huge(1.0)
    noahmp%biochem%param%WDPOOL         = huge(1.0)
    noahmp%biochem%param%LTOVRC         = huge(1.0)
    noahmp%biochem%param%TDLEF          = huge(1.0)
    noahmp%biochem%param%DILEFW         = huge(1.0)
    noahmp%biochem%param%DILEFC         = huge(1.0)
    noahmp%biochem%param%FRAGR          = huge(1.0)
    noahmp%biochem%param%MRP            = huge(1.0)
    noahmp%biochem%param%Q10MR          = huge(1.0)
    noahmp%biochem%param%LFMR25         = huge(1.0)
    noahmp%biochem%param%STMR25         = huge(1.0)
    noahmp%biochem%param%RTMR25         = huge(1.0)
    noahmp%biochem%param%GRAINMR25      = huge(1.0)
    noahmp%biochem%param%FRA_GR         = huge(1.0)
    noahmp%biochem%param%LEFREEZ        = huge(1.0)
    noahmp%biochem%param%BIO2LAI        = huge(1.0)
    noahmp%biochem%param%GDDTBASE       = huge(1.0)
    noahmp%biochem%param%GDDTCUT        = huge(1.0)
    noahmp%biochem%param%GDDS1          = huge(1.0)
    noahmp%biochem%param%GDDS2          = huge(1.0)
    noahmp%biochem%param%GDDS3          = huge(1.0)
    noahmp%biochem%param%GDDS4          = huge(1.0)
    noahmp%biochem%param%GDDS5          = huge(1.0)
    noahmp%biochem%param%I2PAR          = huge(1.0)
    noahmp%biochem%param%TASSIM0        = huge(1.0)
    noahmp%biochem%param%TASSIM1        = huge(1.0)
    noahmp%biochem%param%TASSIM2        = huge(1.0)
    noahmp%biochem%param%AREF           = huge(1.0)
    noahmp%biochem%param%K              = huge(1.0)
    noahmp%biochem%param%EPSI           = huge(1.0)
    noahmp%biochem%param%PSNRF          = huge(1.0)
    noahmp%biochem%param%SLAREA         = huge(1.0)
    noahmp%biochem%param%XSAMIN         = huge(1.0)
    noahmp%biochem%param%BF             = huge(1.0)
    noahmp%biochem%param%WSTRC          = huge(1.0)
    noahmp%biochem%param%LAIMIN         = huge(1.0)
    noahmp%biochem%param%RTOVRC         = huge(1.0)
    noahmp%biochem%param%RSDRYC         = huge(1.0)
    noahmp%biochem%param%RSWOODC        = huge(1.0)

    if( .not. allocated( noahmp%biochem%param%DILE_FC ) ) allocate( noahmp%biochem%param%DILE_FC (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%DILE_FW ) ) allocate( noahmp%biochem%param%DILE_FW (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%LFCT    ) ) allocate( noahmp%biochem%param%LFCT    (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%STCT    ) ) allocate( noahmp%biochem%param%STCT    (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%RTCT    ) ) allocate( noahmp%biochem%param%RTCT    (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%LFPT    ) ) allocate( noahmp%biochem%param%LFPT    (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%STPT    ) ) allocate( noahmp%biochem%param%STPT    (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%RTPT    ) ) allocate( noahmp%biochem%param%RTPT    (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%GRAINPT ) ) allocate( noahmp%biochem%param%GRAINPT (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%LF_OVRC ) ) allocate( noahmp%biochem%param%LF_OVRC (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%ST_OVRC ) ) allocate( noahmp%biochem%param%ST_OVRC (1:NSTAGE) )
    if( .not. allocated( noahmp%biochem%param%RT_OVRC ) ) allocate( noahmp%biochem%param%RT_OVRC (1:NSTAGE) )

    noahmp%biochem%param%DILE_FC(:)     = huge(1.0)
    noahmp%biochem%param%DILE_FW(:)     = huge(1.0)
    noahmp%biochem%param%LFCT(:)        = huge(1.0)
    noahmp%biochem%param%STCT(:)        = huge(1.0)
    noahmp%biochem%param%RTCT(:)        = huge(1.0)
    noahmp%biochem%param%LFPT(:)        = huge(1.0)
    noahmp%biochem%param%STPT(:)        = huge(1.0)
    noahmp%biochem%param%RTPT(:)        = huge(1.0)
    noahmp%biochem%param%GRAINPT(:)     = huge(1.0)
    noahmp%biochem%param%LF_OVRC(:)     = huge(1.0)
    noahmp%biochem%param%ST_OVRC(:)     = huge(1.0)
    noahmp%biochem%param%RT_OVRC(:)     = huge(1.0)

    end associate

  end subroutine BiochemVarInitDefault

!=== initialize with input data or table values

  subroutine BiochemVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    associate(                                                  &
              I           => noahmp%config%domain%ILOC         ,&
              J           => noahmp%config%domain%JLOC         ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              CROPTYP     => noahmp%config%domain%CROPTYP      ,&
              SOILCOLOR   => noahmp%config%domain%SOILCOLOR    ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL        ,&
              NBAND       => noahmp%config%domain%NBAND         &
             )

    ! biochem state variables
    noahmp%biochem%state%PGS            = NoahmpIO%PGSXY   (I,J)   
    noahmp%biochem%state%LFMASS         = NoahmpIO%LFMASSXY(I,J)
    noahmp%biochem%state%RTMASS         = NoahmpIO%RTMASSXY(I,J)
    noahmp%biochem%state%STMASS         = NoahmpIO%STMASSXY(I,J) 
    noahmp%biochem%state%WOOD           = NoahmpIO%WOODXY  (I,J) 
    noahmp%biochem%state%STBLCP         = NoahmpIO%STBLCPXY(I,J) 
    noahmp%biochem%state%FASTCP         = NoahmpIO%FASTCPXY(I,J)
    noahmp%biochem%state%GRAIN          = NoahmpIO%GRAINXY (I,J)  
    noahmp%biochem%state%GDD            = NoahmpIO%GDDXY   (I,J)  

    ! biochem parameter variables
    noahmp%biochem%param%FOLNMX         = NoahmpIO%FOLNMX_TABLE(VEGTYP)
    noahmp%biochem%param%QE25           = NoahmpIO%QE25_TABLE(VEGTYP)
    noahmp%biochem%param%VCMX25         = NoahmpIO%VCMX25_TABLE(VEGTYP)
    noahmp%biochem%param%AVCMX          = NoahmpIO%AVCMX_TABLE(VEGTYP)
    noahmp%biochem%param%C3PSN          = NoahmpIO%C3PSN_TABLE(VEGTYP)
    noahmp%biochem%param%MP             = NoahmpIO%MP_TABLE(VEGTYP)
    noahmp%biochem%param%ARM            = NoahmpIO%ARM_TABLE(VEGTYP)
    noahmp%biochem%param%RMF25          = NoahmpIO%RMF25_TABLE(VEGTYP)
    noahmp%biochem%param%RMS25          = NoahmpIO%RMS25_TABLE(VEGTYP)
    noahmp%biochem%param%RMR25          = NoahmpIO%RMR25_TABLE(VEGTYP)
    noahmp%biochem%param%WRRAT          = NoahmpIO%WRRAT_TABLE(VEGTYP)
    noahmp%biochem%param%WDPOOL         = NoahmpIO%WDPOOL_TABLE(VEGTYP)
    noahmp%biochem%param%LTOVRC         = NoahmpIO%LTOVRC_TABLE(VEGTYP)
    noahmp%biochem%param%TDLEF          = NoahmpIO%TDLEF_TABLE(VEGTYP)
    noahmp%biochem%param%DILEFW         = NoahmpIO%DILEFW_TABLE(VEGTYP)
    noahmp%biochem%param%DILEFC         = NoahmpIO%DILEFC_TABLE(VEGTYP)
    noahmp%biochem%param%FRAGR          = NoahmpIO%FRAGR_TABLE(VEGTYP)
    noahmp%biochem%param%MRP            = NoahmpIO%MRP_TABLE(VEGTYP)
    noahmp%biochem%param%TMIN           = NoahmpIO%TMIN_TABLE(VEGTYP)
    noahmp%biochem%param%SLA            = NoahmpIO%SLA_TABLE(VEGTYP)
    noahmp%biochem%param%DEFAULT_CROP   = NoahmpIO%DEFAULT_CROP_TABLE
    noahmp%biochem%param%XSAMIN         = NoahmpIO%XSAMIN_TABLE(VEGTYP)
    noahmp%biochem%param%BF             = NoahmpIO%BF_TABLE(VEGTYP)
    noahmp%biochem%param%WSTRC          = NoahmpIO%WSTRC_TABLE(VEGTYP)
    noahmp%biochem%param%LAIMIN         = NoahmpIO%LAIMIN_TABLE(VEGTYP)
    noahmp%biochem%param%RTOVRC         = NoahmpIO%RTOVRC_TABLE(VEGTYP)
    noahmp%biochem%param%RSDRYC         = NoahmpIO%RSDRYC_TABLE(VEGTYP)
    noahmp%biochem%param%RSWOODC        = NoahmpIO%RSWOODC_TABLE(VEGTYP)

    if ( CROPTYP > 0 ) then
       noahmp%biochem%param%PLTDAY      = NoahmpIO%PLTDAY_TABLE(CROPTYP)
       noahmp%biochem%param%HSDAY       = NoahmpIO%HSDAY_TABLE(CROPTYP)
       noahmp%biochem%param%C3C4        = NoahmpIO%C3C4_TABLE(CROPTYP)
       noahmp%biochem%param%FOLNMX      = NoahmpIO%FOLNMXI_TABLE(CROPTYP)
       noahmp%biochem%param%QE25        = NoahmpIO%QE25I_TABLE(CROPTYP)
       noahmp%biochem%param%VCMX25      = NoahmpIO%VCMX25I_TABLE(CROPTYP)
       noahmp%biochem%param%AVCMX       = NoahmpIO%AVCMXI_TABLE(CROPTYP)
       noahmp%biochem%param%C3PSN       = NoahmpIO%C3PSNI_TABLE(CROPTYP)
       noahmp%biochem%param%MP          = NoahmpIO%MPI_TABLE(CROPTYP)
       noahmp%biochem%param%FOLN_MX     = NoahmpIO%FOLN_MX_TABLE(CROPTYP)
       noahmp%biochem%param%Q10MR       = NoahmpIO%Q10MR_TABLE(CROPTYP)
       noahmp%biochem%param%LFMR25      = NoahmpIO%LFMR25_TABLE(CROPTYP)
       noahmp%biochem%param%STMR25      = NoahmpIO%STMR25_TABLE(CROPTYP)
       noahmp%biochem%param%RTMR25      = NoahmpIO%RTMR25_TABLE(CROPTYP)
       noahmp%biochem%param%GRAINMR25   = NoahmpIO%GRAINMR25_TABLE(CROPTYP)
       noahmp%biochem%param%FRA_GR      = NoahmpIO%FRA_GR_TABLE(CROPTYP)
       noahmp%biochem%param%LEFREEZ     = NoahmpIO%LEFREEZ_TABLE(CROPTYP)
       noahmp%biochem%param%BIO2LAI     = NoahmpIO%BIO2LAI_TABLE(CROPTYP)
       noahmp%biochem%param%GDDTBASE    = NoahmpIO%GDDTBASE_TABLE(CROPTYP)
       noahmp%biochem%param%GDDTCUT     = NoahmpIO%GDDTCUT_TABLE(CROPTYP)
       noahmp%biochem%param%GDDS1       = NoahmpIO%GDDS1_TABLE(CROPTYP)
       noahmp%biochem%param%GDDS2       = NoahmpIO%GDDS2_TABLE(CROPTYP)
       noahmp%biochem%param%GDDS3       = NoahmpIO%GDDS3_TABLE(CROPTYP)
       noahmp%biochem%param%GDDS4       = NoahmpIO%GDDS4_TABLE(CROPTYP)
       noahmp%biochem%param%GDDS5       = NoahmpIO%GDDS5_TABLE(CROPTYP)
       noahmp%biochem%param%I2PAR       = NoahmpIO%I2PAR_TABLE(CROPTYP)
       noahmp%biochem%param%TASSIM0     = NoahmpIO%TASSIM0_TABLE(CROPTYP)
       noahmp%biochem%param%TASSIM1     = NoahmpIO%TASSIM1_TABLE(CROPTYP)
       noahmp%biochem%param%TASSIM2     = NoahmpIO%TASSIM2_TABLE(CROPTYP)
       noahmp%biochem%param%AREF        = NoahmpIO%AREF_TABLE(CROPTYP)
       noahmp%biochem%param%K           = NoahmpIO%K_TABLE(CROPTYP)
       noahmp%biochem%param%EPSI        = NoahmpIO%EPSI_TABLE(CROPTYP)
       noahmp%biochem%param%PSNRF       = NoahmpIO%PSNRF_TABLE(CROPTYP)
       noahmp%biochem%param%DILE_FC     = NoahmpIO%DILE_FC_TABLE(CROPTYP,:)
       noahmp%biochem%param%DILE_FW     = NoahmpIO%DILE_FW_TABLE(CROPTYP,:)
       noahmp%biochem%param%LFCT        = NoahmpIO%LFCT_TABLE(CROPTYP,:)
       noahmp%biochem%param%STCT        = NoahmpIO%STCT_TABLE(CROPTYP,:)
       noahmp%biochem%param%RTCT        = NoahmpIO%RTCT_TABLE(CROPTYP,:)
       noahmp%biochem%param%LFPT        = NoahmpIO%LFPT_TABLE(CROPTYP,:)
       noahmp%biochem%param%STPT        = NoahmpIO%STPT_TABLE(CROPTYP,:)
       noahmp%biochem%param%RTPT        = NoahmpIO%RTPT_TABLE(CROPTYP,:)
       noahmp%biochem%param%GRAINPT     = NoahmpIO%GRAINPT_TABLE(CROPTYP,:)
       noahmp%biochem%param%LF_OVRC     = NoahmpIO%LF_OVRC_TABLE(CROPTYP,:)
       noahmp%biochem%param%ST_OVRC     = NoahmpIO%ST_OVRC_TABLE(CROPTYP,:)
       noahmp%biochem%param%RT_OVRC     = NoahmpIO%RT_OVRC_TABLE(CROPTYP,:)
    endif

    if(NoahmpIO%iopt_crop == 1 .and. noahmp%config%domain%CROPTYP > 0) then
       noahmp%biochem%param%PLTDAY = NoahmpIO%PLANTING(I,J)
       noahmp%biochem%param%HSDAY  = NoahmpIO%HARVEST (I,J)
       noahmp%biochem%param%GDDS1  = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GDDS1
       noahmp%biochem%param%GDDS2  = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GDDS2
       noahmp%biochem%param%GDDS3  = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GDDS3
       noahmp%biochem%param%GDDS4  = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GDDS4
       noahmp%biochem%param%GDDS5  = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GDDS5
    end if

    if(NoahmpIO%iopt_irr == 2) then
       noahmp%biochem%param%PLTDAY = NoahmpIO%PLANTING(I,J)
       noahmp%biochem%param%HSDAY  = NoahmpIO%HARVEST (I,J)
    end if
    
    noahmp%biochem%state%FOLN = 1.0 ! for now, set to nitrogen saturation
    
    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
