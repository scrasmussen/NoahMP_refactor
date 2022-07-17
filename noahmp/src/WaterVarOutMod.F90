module WaterVarOutMod

!!! Transfer column (1-D) Noah-MP water variables to 2D NoahmpIO for output
!!! Water variables should be first defined in WaterVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output=====

  subroutine WaterVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local loop index
    integer                          :: ISOIL

    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ      ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer     &
             )

    NoahmpIO%SMSTAV   (I,J)            = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SMSTOT   (I,J)            = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SFCRUNOFF(I,J)            = NoahmpIO%SFCRUNOFF(I,J) + (noahmp%water%flux%RunoffSurface * NoahmpIO%DTBL)
    NoahmpIO%UDRUNOFF (I,J)            = NoahmpIO%UDRUNOFF (I,J) + (noahmp%water%flux%RunoffSubsurface * NoahmpIO%DTBL)
    NoahmpIO%QTDRAIN  (I,J)            = NoahmpIO%QTDRAIN  (I,J) + (noahmp%water%flux%TileDrain * NoahmpIO%DTBL)
    NoahmpIO%SNOWC    (I,J)            = noahmp%water%state%SnowCoverFrac
    NoahmpIO%SMOIS    (I,1:NumSoilLayer,J)    = noahmp%water%state%SoilMoisture(1:NumSoilLayer)
    NoahmpIO%SH2O     (I,1:NumSoilLayer,J)    = noahmp%water%state%SoilLiqWater(1:NumSoilLayer)
    NoahmpIO%SNOW     (I,J)            = noahmp%water%state%SnowWaterEquiv
    NoahmpIO%SNOWH    (I,J)            = noahmp%water%state%SnowDepth
    NoahmpIO%CANWAT   (I,J)            = noahmp%water%state%CanopyLiqWater + noahmp%water%state%CanopyIce
    NoahmpIO%ACSNOW   (I,J)            = NoahmpIO%ACSNOW(I,J) + (NoahmpIO%RAINBL (I,J) * noahmp%water%state%FrozenPrecipFrac)
    NoahmpIO%ACSNOM   (I,J)            = NoahmpIO%ACSNOM(I,J) + (noahmp%water%flux%SnowBotOutflow * NoahmpIO%DTBL) + &
                                         noahmp%water%state%PondSfcThinSnwMelt + noahmp%water%state%PondSfcThinSnwComb + &
                                         noahmp%water%state%PondSfcThinSnwTrans

    NoahmpIO%CANLIQXY (I,J)            = noahmp%water%state%CanopyLiqWater
    NoahmpIO%CANICEXY (I,J)            = noahmp%water%state%CanopyIce
    NoahmpIO%FWETXY   (I,J)            = noahmp%water%state%CanopyWetFrac
    NoahmpIO%SNEQVOXY (I,J)            = noahmp%water%state%SnowWaterEquivPrev
    NoahmpIO%QSNOWXY  (I,J)            = noahmp%water%flux%SnowfallGround
    NoahmpIO%QRAINXY  (I,J)            = noahmp%water%flux%RainfallGround
    NoahmpIO%WSLAKEXY (I,J)            = noahmp%water%state%WaterStorageLake
    NoahmpIO%ZWTXY    (I,J)            = noahmp%water%state%WaterTableDepth
    NoahmpIO%WAXY     (I,J)            = noahmp%water%state%WaterStorageAquifer
    NoahmpIO%WTXY     (I,J)            = noahmp%water%state%WaterStorageSoilAqf
    NoahmpIO%SNICEXY  (I,-NumSnowLayerMax+1:0,J) = noahmp%water%state%SnowIce(-NumSnowLayerMax+1:0)
    NoahmpIO%SNLIQXY  (I,-NumSnowLayerMax+1:0,J) = noahmp%water%state%SnowLiqWater(-NumSnowLayerMax+1:0)
    NoahmpIO%RUNSFXY  (I,J)            = noahmp%water%flux%RunoffSurface
    NoahmpIO%RUNSBXY  (I,J)            = noahmp%water%flux%RunoffSubsurface
    NoahmpIO%ECANXY   (I,J)            = noahmp%water%flux%EvapCanopyNet
    NoahmpIO%EDIRXY   (I,J)            = noahmp%water%flux%EvapSoilNet
    NoahmpIO%ETRANXY  (I,J)            = noahmp%water%flux%Transpiration
    NoahmpIO%RECHXY   (I,J)            = NoahmpIO%RECHXY(I,J) + (noahmp%water%state%RechargeGwShallowWT*1.0e3)      !RECHARGE TO THE WATER TABLE
    NoahmpIO%DEEPRECHXY(I,J)           = NoahmpIO%DEEPRECHXY(I,J) + noahmp%water%state%RechargeGwDeepWT
    NoahmpIO%SMCWTDXY (I,J)            = noahmp%water%state%SoilMoistureToWT

! irrigation
    NoahmpIO%IRNUMSI  (I,J)            = noahmp%water%state%IrrigationCntSprinkler
    NoahmpIO%IRNUMMI  (I,J)            = noahmp%water%state%IrrigationCntMicro
    NoahmpIO%IRNUMFI  (I,J)            = noahmp%water%state%IrrigationCntFlood
    NoahmpIO%IRWATSI  (I,J)            = noahmp%water%state%IrrigationAmtSprinkler
    NoahmpIO%IRWATMI  (I,J)            = noahmp%water%state%IrrigationAmtMicro
    NoahmpIO%IRWATFI  (I,J)            = noahmp%water%state%IrrigationAmtFlood
    NoahmpIO%IRSIVOL  (I,J)            = NoahmpIO%IRSIVOL(I,J)+(noahmp%water%flux%IrrigationRateSprinkler*1000.0)
    NoahmpIO%IRMIVOL  (I,J)            = NoahmpIO%IRMIVOL(I,J)+(noahmp%water%flux%IrrigationRateMicro*1000.0)
    NoahmpIO%IRFIVOL  (I,J)            = NoahmpIO%IRFIVOL(I,J)+(noahmp%water%flux%IrrigationRateFlood*1000.0)
    NoahmpIO%IRELOSS  (I,J)            = NoahmpIO%IRELOSS(I,J)+(noahmp%water%flux%EvapIrriSprinkler*NoahmpIO%DTBL) ! mm

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt   (I,J)           = max((noahmp%water%flux%RunoffSurface * NoahmpIO%DTBL), 0.)             ! mm, surface runoff
    NoahmpIO%soldrain  (I,J)           = max((noahmp%water%flux%RunoffSubsurface * NoahmpIO%DTBL), 0.)             ! mm, underground runoff
    NoahmpIO%qtiledrain(I,J)           = max((noahmp%water%flux%TileDrain * NoahmpIO%DTBL), 0.)             ! mm, tile drainage
#endif

    end associate

  end subroutine WaterVarOutTransfer

end module WaterVarOutMod
