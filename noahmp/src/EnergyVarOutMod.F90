module EnergyVarOutMod

!!! Transfer column (1-D) Noah-MP energy variables to 2D NoahmpIO for output
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He, & refactor team (April 27, 2021)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output=====

  subroutine EnergyVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local loop index
    real(kind=kind_noahmp)           :: LAISUN       ! sunlit leaf area index (m2/m2)
    real(kind=kind_noahmp)           :: LAISHA       ! shaded leaf area index (m2/m2)
    real(kind=kind_noahmp)           :: RB           ! leaf boundary layer resistance (s/m)

    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ      ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax  &
             )

             NoahmpIO%TSK      (I,J)         = noahmp%energy%state%TRAD
             NoahmpIO%HFX      (I,J)         = noahmp%energy%flux%FSH
             NoahmpIO%GRDFLX   (I,J)         = noahmp%energy%flux%SSOIL

             if ( noahmp%energy%state%ALBEDO > -999 ) then
                NoahmpIO%ALBEDO(I,J)         = noahmp%energy%state%ALBEDO
             endif

             NoahmpIO%TSLB     (I,1:NumSoilLayer,J) = noahmp%energy%state%STC(1:NumSoilLayer)
             NoahmpIO%TSNOXY   (I,-NumSnowLayerMax+1:0,J) = noahmp%energy%state%STC(-NumSnowLayerMax+1:0)
             NoahmpIO%EMISS    (I,J)         = noahmp%energy%state%EMISSI
             NoahmpIO%QSFC     (I,J)         = noahmp%energy%state%QSFC
             NoahmpIO%TVXY     (I,J)         = noahmp%energy%state%TV
             NoahmpIO%TGXY     (I,J)         = noahmp%energy%state%TG
             NoahmpIO%EAHXY    (I,J)         = noahmp%energy%state%EAH
             NoahmpIO%TAHXY    (I,J)         = noahmp%energy%state%TAH
             NoahmpIO%CMXY     (I,J)         = noahmp%energy%state%CM
             NoahmpIO%CHXY     (I,J)         = noahmp%energy%state%CH
             NoahmpIO%ALBOLDXY (I,J)         = noahmp%energy%state%ALBOLD
             NoahmpIO%LAI      (I,J)         = noahmp%energy%state%LAI
             NoahmpIO%XSAIXY   (I,J)         = noahmp%energy%state%SAI
             NoahmpIO%TAUSSXY  (I,J)         = noahmp%energy%state%TAUSS
             NoahmpIO%Z0       (I,J)         = noahmp%energy%state%Z0WRF
             NoahmpIO%ZNT      (I,J)         = noahmp%energy%state%Z0WRF
             NoahmpIO%T2MVXY   (I,J)         = noahmp%energy%state%T2MV
             NoahmpIO%T2MBXY   (I,J)         = noahmp%energy%state%T2MB
             NoahmpIO%Q2MVXY   (I,J)         = noahmp%energy%state%Q2V/(1.0 - noahmp%energy%state%Q2V)  ! specific humidity to mixing ratio
             NoahmpIO%Q2MBXY   (I,J)         = noahmp%energy%state%Q2B/(1.0 - noahmp%energy%state%Q2B)  ! consistent with registry def of Q2
             NoahmpIO%TRADXY   (I,J)         = noahmp%energy%state%TRAD
             NoahmpIO%FVEGXY   (I,J)         = noahmp%energy%state%FVEG
             NoahmpIO%FSAXY    (I,J)         = noahmp%energy%flux%FSA
             NoahmpIO%FIRAXY   (I,J)         = noahmp%energy%flux%FIRA
             NoahmpIO%APARXY   (I,J)         = noahmp%energy%flux%APAR
             NoahmpIO%SAVXY    (I,J)         = noahmp%energy%flux%SAV
             NoahmpIO%SAGXY    (I,J)         = noahmp%energy%flux%SAG
             NoahmpIO%RSSUNXY  (I,J)         = noahmp%energy%state%RSSUN
             NoahmpIO%RSSHAXY  (I,J)         = noahmp%energy%state%RSSHA
             LAISUN                          = max(noahmp%energy%state%LAISUN, 0.0)
             LAISHA                          = max(noahmp%energy%state%LAISHA, 0.0)
             RB                              = max(noahmp%energy%state%RB,     0.0)

! -- New Calculation of total Canopy/Stomatal Conductance Based on Bonan et al. (2011)
! -- Inverse of Canopy Resistance (below)

             if(noahmp%energy%state%RSSUN  .le. 0.0 .or. noahmp%energy%state%RSSHA  .le. 0.0 &
                .or. LAISUN .eq. 0.0 .or. LAISHA .eq. 0.0 .or. noahmp%energy%state%RSSUN .eq. undefined_real &
                .or. noahmp%energy%state%RSSHA .eq. undefined_real ) then
                NoahmpIO%RS    (I,J)   = 0.0
             else
                NoahmpIO%RS    (I,J)   = ((1.0/(noahmp%energy%state%RSSUN+RB)*noahmp%energy%state%LAISUN) + &
                                         ((1.0/(noahmp%energy%state%RSSHA+RB))*noahmp%energy%state%LAISHA))
                NoahmpIO%RS    (I,J)   = 1.0/NoahmpIO%RS (I,J) !Resistance
             endif

             NoahmpIO%BGAPXY   (I,J)   = noahmp%energy%state%BGAP
             NoahmpIO%WGAPXY   (I,J)   = noahmp%energy%state%WGAP
             NoahmpIO%TGVXY    (I,J)   = noahmp%energy%state%TGV
             NoahmpIO%TGBXY    (I,J)   = noahmp%energy%state%TGB
             NoahmpIO%CHVXY    (I,J)   = noahmp%energy%state%CHV
             NoahmpIO%CHBXY    (I,J)   = noahmp%energy%state%CHB
             NoahmpIO%IRCXY    (I,J)   = noahmp%energy%flux%IRC
             NoahmpIO%IRGXY    (I,J)   = noahmp%energy%flux%IRG
             NoahmpIO%SHCXY    (I,J)   = noahmp%energy%flux%SHC
             NoahmpIO%SHGXY    (I,J)   = noahmp%energy%flux%SHG
             NoahmpIO%EVGXY    (I,J)   = noahmp%energy%flux%EVG
             NoahmpIO%GHVXY    (I,J)   = noahmp%energy%flux%GHV
             NoahmpIO%IRBXY    (I,J)   = noahmp%energy%flux%IRB
             NoahmpIO%SHBXY    (I,J)   = noahmp%energy%flux%SHB
             NoahmpIO%EVBXY    (I,J)   = noahmp%energy%flux%EVB
             NoahmpIO%GHBXY    (I,J)   = noahmp%energy%flux%GHB
             NoahmpIO%TRXY     (I,J)   = noahmp%energy%flux%TR
             NoahmpIO%EVCXY    (I,J)   = noahmp%energy%flux%EVC
             NoahmpIO%CHLEAFXY (I,J)   = noahmp%energy%state%CHLEAF
             NoahmpIO%CHUCXY   (I,J)   = noahmp%energy%state%CHUC
             NoahmpIO%CHV2XY   (I,J)   = noahmp%energy%state%CHV2
             NoahmpIO%CHB2XY   (I,J)   = noahmp%energy%state%EHB2
             NoahmpIO%IRRSPLH  (I,J)   = NoahmpIO%IRRSPLH(I,J) + &
                                         (noahmp%energy%flux%FIRR*noahmp%config%domain%MainTimeStep) ! Joules/m^2

    end associate

  end subroutine EnergyVarOutTransfer

end module EnergyVarOutMod
