module EnergyVarOutMod

!!! Transfer column (1-D) Noah-MP energy variables to 2D NoahmpIO for output

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He, & refactor team (July 2022)
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
    real(kind=kind_noahmp)           :: LeafAreaIndSunlit       ! sunlit leaf area index (m2/m2)
    real(kind=kind_noahmp)           :: LeafAreaIndShade       ! shaded leaf area index (m2/m2)
    real(kind=kind_noahmp)           :: ResistanceLeafBoundary           ! leaf boundary layer resistance (s/m)

    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ      ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax  &
             )

             NoahmpIO%TSK      (I,J)         = noahmp%energy%state%TemperatureRadSfc
             NoahmpIO%HFX      (I,J)         = noahmp%energy%flux%HeatSensibleTot
             NoahmpIO%GRDFLX   (I,J)         = noahmp%energy%flux%HeatGroundTot

             if ( noahmp%energy%state%AlbedoSfc > -999 ) then
                NoahmpIO%ALBEDO(I,J)         = noahmp%energy%state%AlbedoSfc
             endif

             NoahmpIO%TSLB     (I,1:NumSoilLayer,J) = noahmp%energy%state%TemperatureSoilSnow(1:NumSoilLayer)
             NoahmpIO%TSNOXY   (I,-NumSnowLayerMax+1:0,J) = noahmp%energy%state%TemperatureSoilSnow(-NumSnowLayerMax+1:0)
             NoahmpIO%EMISS    (I,J)         = noahmp%energy%state%EmissivitySfc
             NoahmpIO%QSFC     (I,J)         = noahmp%energy%state%SpecHumiditySfcBare
             NoahmpIO%TVXY     (I,J)         = noahmp%energy%state%TemperatureCanopy
             NoahmpIO%TGXY     (I,J)         = noahmp%energy%state%TemperatureGrd
             NoahmpIO%EAHXY    (I,J)         = noahmp%energy%state%PressureVaporCanAir
             NoahmpIO%TAHXY    (I,J)         = noahmp%energy%state%TemperatureCanopyAir
             NoahmpIO%CMXY     (I,J)         = noahmp%energy%state%CM
             NoahmpIO%CHXY     (I,J)         = noahmp%energy%state%CH
             NoahmpIO%ALBOLDXY (I,J)         = noahmp%energy%state%AlbedoSnowPrev
             NoahmpIO%LAI      (I,J)         = noahmp%energy%state%LeafAreaIndex
             NoahmpIO%XSAIXY   (I,J)         = noahmp%energy%state%StemAreaIndex
             NoahmpIO%TAUSSXY  (I,J)         = noahmp%energy%state%SnowAgeNondim
             NoahmpIO%Z0       (I,J)         = noahmp%energy%state%RoughLenMomSfcToAtm
             NoahmpIO%ZNT      (I,J)         = noahmp%energy%state%RoughLenMomSfcToAtm
             NoahmpIO%T2MVXY   (I,J)         = noahmp%energy%state%TemperatureAir2mVeg
             NoahmpIO%T2MBXY   (I,J)         = noahmp%energy%state%TemperatureAir2mBare
             NoahmpIO%Q2MVXY   (I,J)         = noahmp%energy%state%SpecHumidity2mVeg/(1.0 - noahmp%energy%state%SpecHumidity2mVeg)  ! specific humidity to mixing ratio
             NoahmpIO%Q2MBXY   (I,J)         = noahmp%energy%state%SpecHumidity2mBare/(1.0 - noahmp%energy%state%SpecHumidity2mBare)  ! consistent with registry def of Q2
             NoahmpIO%TRADXY   (I,J)         = noahmp%energy%state%TemperatureRadSfc
             NoahmpIO%FVEGXY   (I,J)         = noahmp%energy%state%VegFrac
             NoahmpIO%FSAXY    (I,J)         = noahmp%energy%flux%RadSwAbsTot
             NoahmpIO%FIRAXY   (I,J)         = noahmp%energy%flux%RadLwNetTot
             NoahmpIO%APARXY   (I,J)         = noahmp%energy%flux%RadPhotoActAbsCan
             NoahmpIO%SAVXY    (I,J)         = noahmp%energy%flux%RadSwAbsVeg
             NoahmpIO%SAGXY    (I,J)         = noahmp%energy%flux%RadSwAbsGrd
             NoahmpIO%RSSUNXY  (I,J)         = noahmp%energy%state%ResistanceStomataSunlit
             NoahmpIO%RSSHAXY  (I,J)         = noahmp%energy%state%ResistanceStomataShade
             LeafAreaIndSunlit               = max(noahmp%energy%state%LeafAreaIndSunlit, 0.0)
             LeafAreaIndShade                = max(noahmp%energy%state%LeafAreaIndShade, 0.0)
             ResistanceLeafBoundary               = max(noahmp%energy%state%ResistanceLeafBoundary, 0.0)

! -- New Calculation of total Canopy/Stomatal Conductance Based on Bonan et al. (2011)
! -- Inverse of Canopy Resistance (below)

             if(noahmp%energy%state%ResistanceStomataSunlit  .le. 0.0 .or. noahmp%energy%state%ResistanceStomataShade  .le. 0.0 &
                .or. LeafAreaIndSunlit .eq. 0.0 .or. LeafAreaIndShade .eq. 0.0 .or. noahmp%energy%state%ResistanceStomataSunlit .eq. undefined_real &
                .or. noahmp%energy%state%ResistanceStomataShade .eq. undefined_real ) then
                NoahmpIO%RS    (I,J)   = 0.0
             else
                NoahmpIO%RS    (I,J)   = ((1.0/(noahmp%energy%state%ResistanceStomataSunlit+ResistanceLeafBoundary)*noahmp%energy%state%LeafAreaIndSunlit) + &
                                         ((1.0/(noahmp%energy%state%ResistanceStomataShade+ResistanceLeafBoundary))*noahmp%energy%state%LeafAreaIndShade))
                NoahmpIO%RS    (I,J)   = 1.0/NoahmpIO%RS (I,J) !Resistance
             endif

             NoahmpIO%BGAPXY   (I,J)   = noahmp%energy%state%GapBtwCanopy
             NoahmpIO%WGAPXY   (I,J)   = noahmp%energy%state%GapInCanopy
             NoahmpIO%TGVXY    (I,J)   = noahmp%energy%state%TemperatureGrdVeg
             NoahmpIO%TGBXY    (I,J)   = noahmp%energy%state%TemperatureGrdBare
             NoahmpIO%CHVXY    (I,J)   = noahmp%energy%state%CHV
             NoahmpIO%CHBXY    (I,J)   = noahmp%energy%state%CHB
             NoahmpIO%IRCXY    (I,J)   = noahmp%energy%flux%RadLwNetCanopy
             NoahmpIO%IRGXY    (I,J)   = noahmp%energy%flux%RadLwNetVegGrd
             NoahmpIO%SHCXY    (I,J)   = noahmp%energy%flux%HeatSensibleCanopy
             NoahmpIO%SHGXY    (I,J)   = noahmp%energy%flux%HeatSensibleVegGrd
             NoahmpIO%EVGXY    (I,J)   = noahmp%energy%flux%HeatLatentVegGrd
             NoahmpIO%GHVXY    (I,J)   = noahmp%energy%flux%HeatGroundVegGrd
             NoahmpIO%IRBXY    (I,J)   = noahmp%energy%flux%RadLwNetBareGrd
             NoahmpIO%SHBXY    (I,J)   = noahmp%energy%flux%HeatSensibleBareGrd
             NoahmpIO%EVBXY    (I,J)   = noahmp%energy%flux%HeatLatentBareGrd
             NoahmpIO%GHBXY    (I,J)   = noahmp%energy%flux%HeatGroundBareGrd
             NoahmpIO%TRXY     (I,J)   = noahmp%energy%flux%HeatLatentCanTransp
             NoahmpIO%EVCXY    (I,J)   = noahmp%energy%flux%HeatLatentCanEvap
             NoahmpIO%CHLEAFXY (I,J)   = noahmp%energy%state%CHLEAF
             NoahmpIO%CHUCXY   (I,J)   = noahmp%energy%state%CHUC
             NoahmpIO%CHV2XY   (I,J)   = noahmp%energy%state%CHV2
             NoahmpIO%CHB2XY   (I,J)   = noahmp%energy%state%EHB2
             NoahmpIO%IRRSPLH  (I,J)   = NoahmpIO%IRRSPLH(I,J) + &
                                         (noahmp%energy%flux%HeatLatentIrriEvap*noahmp%config%domain%MainTimeStep) ! Joules/m^2

    end associate

  end subroutine EnergyVarOutTransfer

end module EnergyVarOutMod
