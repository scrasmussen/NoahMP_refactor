module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine BiochemVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate( NumCropGrowStage => noahmp%config%domain%NumCropGrowStage )

    ! biochem state variables
    noahmp%biochem%state%PGS            = undefined_int
    noahmp%biochem%state%IPA            = undefined_int
    noahmp%biochem%state%IHA            = undefined_int
    noahmp%biochem%state%IGS            = undefined_real
    noahmp%biochem%state%FOLN           = undefined_real
    noahmp%biochem%state%LFMASS         = undefined_real
    noahmp%biochem%state%RTMASS         = undefined_real
    noahmp%biochem%state%STMASS         = undefined_real
    noahmp%biochem%state%WOOD           = undefined_real
    noahmp%biochem%state%STBLCP         = undefined_real
    noahmp%biochem%state%FASTCP         = undefined_real
    noahmp%biochem%state%TOTSC          = undefined_real
    noahmp%biochem%state%TOTLB          = undefined_real
    noahmp%biochem%state%LAPM           = undefined_real
    noahmp%biochem%state%SAPM           = undefined_real
    noahmp%biochem%state%LFMSMN         = undefined_real
    noahmp%biochem%state%STMSMN         = undefined_real
    noahmp%biochem%state%ADDNPPLF       = undefined_real
    noahmp%biochem%state%ADDNPPST       = undefined_real
    noahmp%biochem%state%LEAFPT         = undefined_real
    noahmp%biochem%state%WOODF          = undefined_real
    noahmp%biochem%state%NONLEF         = undefined_real
    noahmp%biochem%state%ROOTPT         = undefined_real
    noahmp%biochem%state%WOODPT         = undefined_real
    noahmp%biochem%state%STEMPT         = undefined_real
    noahmp%biochem%state%FSW            = undefined_real
    noahmp%biochem%state%FST            = undefined_real
    noahmp%biochem%state%FNF            = undefined_real
    noahmp%biochem%state%TF             = undefined_real
    noahmp%biochem%state%RF             = undefined_real
    noahmp%biochem%state%GRAIN          = undefined_real
    noahmp%biochem%state%GDD            = undefined_real

    ! biochem flux variables
    noahmp%biochem%flux%PSNSUN          = undefined_real
    noahmp%biochem%flux%PSNSHA          = undefined_real
    noahmp%biochem%flux%PSN             = undefined_real
    noahmp%biochem%flux%GPP             = undefined_real
    noahmp%biochem%flux%NPP             = undefined_real
    noahmp%biochem%flux%NEE             = undefined_real
    noahmp%biochem%flux%AUTORS          = undefined_real
    noahmp%biochem%flux%HETERS          = undefined_real
    noahmp%biochem%flux%CFLUX           = undefined_real
    noahmp%biochem%flux%NPPL            = undefined_real
    noahmp%biochem%flux%NPPR            = undefined_real
    noahmp%biochem%flux%NPPW            = undefined_real
    noahmp%biochem%flux%NPPS            = undefined_real
    noahmp%biochem%flux%GRLEAF          = undefined_real
    noahmp%biochem%flux%GRROOT          = undefined_real
    noahmp%biochem%flux%GRWOOD          = undefined_real
    noahmp%biochem%flux%GRSTEM          = undefined_real
    noahmp%biochem%flux%LFDEL           = undefined_real
    noahmp%biochem%flux%STDEL           = undefined_real
    noahmp%biochem%flux%STABLC          = undefined_real
    noahmp%biochem%flux%RESP            = undefined_real
    noahmp%biochem%flux%RSSTEM          = undefined_real
    noahmp%biochem%flux%GRGRAIN         = undefined_real
    noahmp%biochem%flux%NPPG            = undefined_real
    noahmp%biochem%flux%RTCONVERT       = undefined_real
    noahmp%biochem%flux%STCONVERT       = undefined_real
    noahmp%biochem%flux%PSNCROP         = undefined_real
    noahmp%biochem%flux%RSWOOD          = undefined_real
    noahmp%biochem%flux%RSLEAF          = undefined_real
    noahmp%biochem%flux%RSROOT          = undefined_real
    noahmp%biochem%flux%DIELF           = undefined_real
    noahmp%biochem%flux%DIEST           = undefined_real
    noahmp%biochem%flux%CARBFX          = undefined_real
    noahmp%biochem%flux%LFTOVR          = undefined_real
    noahmp%biochem%flux%STTOVR          = undefined_real
    noahmp%biochem%flux%WDTOVR          = undefined_real
    noahmp%biochem%flux%RSSOIL          = undefined_real
    noahmp%biochem%flux%RTTOVR          = undefined_real
    noahmp%biochem%flux%CBHYDRAFX       = undefined_real
    noahmp%biochem%flux%GRTOVR          = undefined_real
    noahmp%biochem%flux%LFCONVERT       = undefined_real
    noahmp%biochem%flux%RSGRAIN         = undefined_real
 
    ! biochem parameter variables
    noahmp%biochem%param%DEFAULT_CROP   = undefined_int
    noahmp%biochem%param%PLTDAY         = undefined_int
    noahmp%biochem%param%HSDAY          = undefined_int
    noahmp%biochem%param%C3C4           = undefined_int
    noahmp%biochem%param%FOLNMX         = undefined_real
    noahmp%biochem%param%QE25           = undefined_real
    noahmp%biochem%param%VCMX25         = undefined_real
    noahmp%biochem%param%AVCMX          = undefined_real
    noahmp%biochem%param%C3PSN          = undefined_real
    noahmp%biochem%param%MP             = undefined_real
    noahmp%biochem%param%TMIN           = undefined_real
    noahmp%biochem%param%SLA            = undefined_real
    noahmp%biochem%param%FOLN_MX        = undefined_real
    noahmp%biochem%param%ARM            = undefined_real
    noahmp%biochem%param%RMF25          = undefined_real
    noahmp%biochem%param%RMS25          = undefined_real
    noahmp%biochem%param%RMR25          = undefined_real
    noahmp%biochem%param%WRRAT          = undefined_real
    noahmp%biochem%param%WDPOOL         = undefined_real
    noahmp%biochem%param%LTOVRC         = undefined_real
    noahmp%biochem%param%TDLEF          = undefined_real
    noahmp%biochem%param%DILEFW         = undefined_real
    noahmp%biochem%param%DILEFC         = undefined_real
    noahmp%biochem%param%FRAGR          = undefined_real
    noahmp%biochem%param%MRP            = undefined_real
    noahmp%biochem%param%Q10MR          = undefined_real
    noahmp%biochem%param%LFMR25         = undefined_real
    noahmp%biochem%param%STMR25         = undefined_real
    noahmp%biochem%param%RTMR25         = undefined_real
    noahmp%biochem%param%GRAINMR25      = undefined_real
    noahmp%biochem%param%FRA_GR         = undefined_real
    noahmp%biochem%param%LEFREEZ        = undefined_real
    noahmp%biochem%param%BIO2LAI        = undefined_real
    noahmp%biochem%param%GDDTBASE       = undefined_real
    noahmp%biochem%param%GDDTCUT        = undefined_real
    noahmp%biochem%param%GDDS1          = undefined_real
    noahmp%biochem%param%GDDS2          = undefined_real
    noahmp%biochem%param%GDDS3          = undefined_real
    noahmp%biochem%param%GDDS4          = undefined_real
    noahmp%biochem%param%GDDS5          = undefined_real
    noahmp%biochem%param%I2PAR          = undefined_real
    noahmp%biochem%param%TASSIM0        = undefined_real
    noahmp%biochem%param%TASSIM1        = undefined_real
    noahmp%biochem%param%TASSIM2        = undefined_real
    noahmp%biochem%param%AREF           = undefined_real
    noahmp%biochem%param%K              = undefined_real
    noahmp%biochem%param%EPSI           = undefined_real
    noahmp%biochem%param%PSNRF          = undefined_real
    noahmp%biochem%param%SLAREA         = undefined_real
    noahmp%biochem%param%XSAMIN         = undefined_real
    noahmp%biochem%param%BF             = undefined_real
    noahmp%biochem%param%WSTRC          = undefined_real
    noahmp%biochem%param%LAIMIN         = undefined_real
    noahmp%biochem%param%RTOVRC         = undefined_real
    noahmp%biochem%param%RSDRYC         = undefined_real
    noahmp%biochem%param%RSWOODC        = undefined_real

    if( .not. allocated( noahmp%biochem%param%DILE_FC ) ) allocate( noahmp%biochem%param%DILE_FC (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%DILE_FW ) ) allocate( noahmp%biochem%param%DILE_FW (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%LFCT    ) ) allocate( noahmp%biochem%param%LFCT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%STCT    ) ) allocate( noahmp%biochem%param%STCT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%RTCT    ) ) allocate( noahmp%biochem%param%RTCT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%LFPT    ) ) allocate( noahmp%biochem%param%LFPT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%STPT    ) ) allocate( noahmp%biochem%param%STPT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%RTPT    ) ) allocate( noahmp%biochem%param%RTPT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%GRAINPT ) ) allocate( noahmp%biochem%param%GRAINPT (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%LF_OVRC ) ) allocate( noahmp%biochem%param%LF_OVRC (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%ST_OVRC ) ) allocate( noahmp%biochem%param%ST_OVRC (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%RT_OVRC ) ) allocate( noahmp%biochem%param%RT_OVRC (1:NumCropGrowStage) )

    noahmp%biochem%param%DILE_FC(:)     = undefined_real
    noahmp%biochem%param%DILE_FW(:)     = undefined_real
    noahmp%biochem%param%LFCT(:)        = undefined_real
    noahmp%biochem%param%STCT(:)        = undefined_real
    noahmp%biochem%param%RTCT(:)        = undefined_real
    noahmp%biochem%param%LFPT(:)        = undefined_real
    noahmp%biochem%param%STPT(:)        = undefined_real
    noahmp%biochem%param%RTPT(:)        = undefined_real
    noahmp%biochem%param%GRAINPT(:)     = undefined_real
    noahmp%biochem%param%LF_OVRC(:)     = undefined_real
    noahmp%biochem%param%ST_OVRC(:)     = undefined_real
    noahmp%biochem%param%RT_OVRC(:)     = undefined_real

    end associate

  end subroutine BiochemVarInitDefault

!=== initialize with input data or table values

  subroutine BiochemVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    associate(                                                  &
              I           => noahmp%config%domain%GridIndexI   ,&
              J           => noahmp%config%domain%GridIndexJ   ,&
              VegType     => noahmp%config%domain%VegType      ,&
              CropType    => noahmp%config%domain%CropType      &
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
    noahmp%biochem%param%FOLNMX         = NoahmpIO%FOLNMX_TABLE(VegType)
    noahmp%biochem%param%QE25           = NoahmpIO%QE25_TABLE(VegType)
    noahmp%biochem%param%VCMX25         = NoahmpIO%VCMX25_TABLE(VegType)
    noahmp%biochem%param%AVCMX          = NoahmpIO%AVCMX_TABLE(VegType)
    noahmp%biochem%param%C3PSN          = NoahmpIO%C3PSN_TABLE(VegType)
    noahmp%biochem%param%MP             = NoahmpIO%MP_TABLE(VegType)
    noahmp%biochem%param%ARM            = NoahmpIO%ARM_TABLE(VegType)
    noahmp%biochem%param%RMF25          = NoahmpIO%RMF25_TABLE(VegType)
    noahmp%biochem%param%RMS25          = NoahmpIO%RMS25_TABLE(VegType)
    noahmp%biochem%param%RMR25          = NoahmpIO%RMR25_TABLE(VegType)
    noahmp%biochem%param%WRRAT          = NoahmpIO%WRRAT_TABLE(VegType)
    noahmp%biochem%param%WDPOOL         = NoahmpIO%WDPOOL_TABLE(VegType)
    noahmp%biochem%param%LTOVRC         = NoahmpIO%LTOVRC_TABLE(VegType)
    noahmp%biochem%param%TDLEF          = NoahmpIO%TDLEF_TABLE(VegType)
    noahmp%biochem%param%DILEFW         = NoahmpIO%DILEFW_TABLE(VegType)
    noahmp%biochem%param%DILEFC         = NoahmpIO%DILEFC_TABLE(VegType)
    noahmp%biochem%param%FRAGR          = NoahmpIO%FRAGR_TABLE(VegType)
    noahmp%biochem%param%MRP            = NoahmpIO%MRP_TABLE(VegType)
    noahmp%biochem%param%TMIN           = NoahmpIO%TMIN_TABLE(VegType)
    noahmp%biochem%param%SLA            = NoahmpIO%SLA_TABLE(VegType)
    noahmp%biochem%param%DEFAULT_CROP   = NoahmpIO%DEFAULT_CROP_TABLE
    noahmp%biochem%param%XSAMIN         = NoahmpIO%XSAMIN_TABLE(VegType)
    noahmp%biochem%param%BF             = NoahmpIO%BF_TABLE(VegType)
    noahmp%biochem%param%WSTRC          = NoahmpIO%WSTRC_TABLE(VegType)
    noahmp%biochem%param%LAIMIN         = NoahmpIO%LAIMIN_TABLE(VegType)
    noahmp%biochem%param%RTOVRC         = NoahmpIO%RTOVRC_TABLE(VegType)
    noahmp%biochem%param%RSDRYC         = NoahmpIO%RSDRYC_TABLE(VegType)
    noahmp%biochem%param%RSWOODC        = NoahmpIO%RSWOODC_TABLE(VegType)

    if ( CropType > 0 ) then
       noahmp%biochem%param%PLTDAY      = NoahmpIO%PLTDAY_TABLE(CropType)
       noahmp%biochem%param%HSDAY       = NoahmpIO%HSDAY_TABLE(CropType)
       noahmp%biochem%param%C3C4        = NoahmpIO%C3C4_TABLE(CropType)
       noahmp%biochem%param%FOLNMX      = NoahmpIO%FOLNMXI_TABLE(CropType)
       noahmp%biochem%param%QE25        = NoahmpIO%QE25I_TABLE(CropType)
       noahmp%biochem%param%VCMX25      = NoahmpIO%VCMX25I_TABLE(CropType)
       noahmp%biochem%param%AVCMX       = NoahmpIO%AVCMXI_TABLE(CropType)
       noahmp%biochem%param%C3PSN       = NoahmpIO%C3PSNI_TABLE(CropType)
       noahmp%biochem%param%MP          = NoahmpIO%MPI_TABLE(CropType)
       noahmp%biochem%param%FOLN_MX     = NoahmpIO%FOLN_MX_TABLE(CropType)
       noahmp%biochem%param%Q10MR       = NoahmpIO%Q10MR_TABLE(CropType)
       noahmp%biochem%param%LFMR25      = NoahmpIO%LFMR25_TABLE(CropType)
       noahmp%biochem%param%STMR25      = NoahmpIO%STMR25_TABLE(CropType)
       noahmp%biochem%param%RTMR25      = NoahmpIO%RTMR25_TABLE(CropType)
       noahmp%biochem%param%GRAINMR25   = NoahmpIO%GRAINMR25_TABLE(CropType)
       noahmp%biochem%param%FRA_GR      = NoahmpIO%FRA_GR_TABLE(CropType)
       noahmp%biochem%param%LEFREEZ     = NoahmpIO%LEFREEZ_TABLE(CropType)
       noahmp%biochem%param%BIO2LAI     = NoahmpIO%BIO2LAI_TABLE(CropType)
       noahmp%biochem%param%GDDTBASE    = NoahmpIO%GDDTBASE_TABLE(CropType)
       noahmp%biochem%param%GDDTCUT     = NoahmpIO%GDDTCUT_TABLE(CropType)
       noahmp%biochem%param%GDDS1       = NoahmpIO%GDDS1_TABLE(CropType)
       noahmp%biochem%param%GDDS2       = NoahmpIO%GDDS2_TABLE(CropType)
       noahmp%biochem%param%GDDS3       = NoahmpIO%GDDS3_TABLE(CropType)
       noahmp%biochem%param%GDDS4       = NoahmpIO%GDDS4_TABLE(CropType)
       noahmp%biochem%param%GDDS5       = NoahmpIO%GDDS5_TABLE(CropType)
       noahmp%biochem%param%I2PAR       = NoahmpIO%I2PAR_TABLE(CropType)
       noahmp%biochem%param%TASSIM0     = NoahmpIO%TASSIM0_TABLE(CropType)
       noahmp%biochem%param%TASSIM1     = NoahmpIO%TASSIM1_TABLE(CropType)
       noahmp%biochem%param%TASSIM2     = NoahmpIO%TASSIM2_TABLE(CropType)
       noahmp%biochem%param%AREF        = NoahmpIO%AREF_TABLE(CropType)
       noahmp%biochem%param%K           = NoahmpIO%K_TABLE(CropType)
       noahmp%biochem%param%EPSI        = NoahmpIO%EPSI_TABLE(CropType)
       noahmp%biochem%param%PSNRF       = NoahmpIO%PSNRF_TABLE(CropType)
       noahmp%biochem%param%DILE_FC     = NoahmpIO%DILE_FC_TABLE(CropType,:)
       noahmp%biochem%param%DILE_FW     = NoahmpIO%DILE_FW_TABLE(CropType,:)
       noahmp%biochem%param%LFCT        = NoahmpIO%LFCT_TABLE(CropType,:)
       noahmp%biochem%param%STCT        = NoahmpIO%STCT_TABLE(CropType,:)
       noahmp%biochem%param%RTCT        = NoahmpIO%RTCT_TABLE(CropType,:)
       noahmp%biochem%param%LFPT        = NoahmpIO%LFPT_TABLE(CropType,:)
       noahmp%biochem%param%STPT        = NoahmpIO%STPT_TABLE(CropType,:)
       noahmp%biochem%param%RTPT        = NoahmpIO%RTPT_TABLE(CropType,:)
       noahmp%biochem%param%GRAINPT     = NoahmpIO%GRAINPT_TABLE(CropType,:)
       noahmp%biochem%param%LF_OVRC     = NoahmpIO%LF_OVRC_TABLE(CropType,:)
       noahmp%biochem%param%ST_OVRC     = NoahmpIO%ST_OVRC_TABLE(CropType,:)
       noahmp%biochem%param%RT_OVRC     = NoahmpIO%RT_OVRC_TABLE(CropType,:)
    endif

    if((noahmp%config%nmlist%OptCropModel == 1) .and. (noahmp%config%domain%CropType > 0)) then
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

    if(noahmp%config%nmlist%OptIrrigation == 2) then
       noahmp%biochem%param%PLTDAY = NoahmpIO%PLANTING(I,J)
       noahmp%biochem%param%HSDAY  = NoahmpIO%HARVEST (I,J)
    end if
    
    noahmp%biochem%state%FOLN = 1.0 ! for now, set to nitrogen saturation
    
    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
