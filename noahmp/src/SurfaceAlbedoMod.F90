module SurfaceAlbedoMod

!!! Compute total surface albedo and vegetation radiative fluxes 
!!! per unit incoming direct and diffuse radiation and sunlit fraction of canopy

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowAgingBatsMod,            only : SnowAgingBats
  use SnowAlbedoBatsMod,           only : SnowAlbedoBats
  use SnowAlbedoClassMod,          only : SnowAlbedoClass
  use GroundAlbedoMod,             only : GroundAlbedo
  use CanopyRadiationTwoStreamMod, only : CanopyRadiationTwoStream

  implicit none

contains

  subroutine SurfaceAlbedo(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ALBEDO
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB       ! waveband indices
    integer                          :: IC       ! direct beam: IC=0; diffuse: IC=1
    real(kind=kind_noahmp)           :: WL       ! fraction of LAI+SAI that is LAI
    real(kind=kind_noahmp)           :: WS       ! fraction of LAI+SAI that is SAI
    real(kind=kind_noahmp)           :: MPE      ! prevents overflow for division by zero
    real(kind=kind_noahmp)           :: EXT      ! optical depth direct beam per unit leaf + stem area

! --------------------------------------------------------------------
    associate(                                                        &
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,    number of solar radiation wave bands
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              OptSnowAlbedo   => noahmp%config%nmlist%OptSnowAlbedo  ,& ! in,    options for ground snow surface albedo
              RHOL            => noahmp%energy%param%RHOL            ,& ! in,    leaf reflectance: 1=vis, 2=nir
              RHOS            => noahmp%energy%param%RHOS            ,& ! in,    stem reflectance: 1=vis, 2=nir
              TAUL            => noahmp%energy%param%TAUL            ,& ! in,    leaf transmittance: 1=vis, 2=nir
              TAUS            => noahmp%energy%param%TAUS            ,& ! in,    stem transmittance: 1=vis, 2=nir
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! out,   ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! out,   ground albedo (diffuse: vis, nir)
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! out,   snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! out,   snow albedo for diffuse(1=vis, 2=nir)
              ALBD            => noahmp%energy%state%ALBD            ,& ! out,   surface albedo (direct)
              ALBI            => noahmp%energy%state%ALBI            ,& ! out,   surface albedo (diffuse)
              FSUN            => noahmp%energy%state%FSUN            ,& ! out,   sunlit fraction of canopy
              FSHA            => noahmp%energy%state%FSHA            ,& ! out,   shaded fraction of canopy
              LAISUN          => noahmp%energy%state%LAISUN          ,& ! out,   sunlit leaf area
              LAISHA          => noahmp%energy%state%LAISHA          ,& ! out,   shaded leaf area
              BGAP            => noahmp%energy%state%BGAP            ,& ! out,   between canopy gap fraction for beam
              WGAP            => noahmp%energy%state%WGAP            ,& ! out,   within canopy gap fraction for beam
              RHO             => noahmp%energy%state%RHO             ,& ! out,   leaf/stem reflectance weighted by fraction LAI and SAI
              TAU             => noahmp%energy%state%TAU             ,& ! out,   leaf/stem transmittance weighted by fraction LAI and SAI
              VAI             => noahmp%energy%state%VAI             ,& ! out,   one-sided leaf+stem area index (m2/m2)
              GDIR            => noahmp%energy%state%GDIR            ,& ! out,   projected leaf+stem area in solar direction
              RadSwAbsVegDir            => noahmp%energy%flux%RadSwAbsVegDir             ,& ! out,   flux abs by veg (per unit direct flux)
              RadSwAbsVegDif            => noahmp%energy%flux%RadSwAbsVegDif             ,& ! out,   flux abs by veg (per unit diffuse flux)
              RadSwDirTranGrdDir            => noahmp%energy%flux%RadSwDirTranGrdDir             ,& ! out,   down direct flux below veg (per unit dir flux)
              RadSwDifTranGrdDir            => noahmp%energy%flux%RadSwDifTranGrdDir             ,& ! out,   down diffuse flux below veg (per unit dir flux)
              RadSwDifTranGrdDif            => noahmp%energy%flux%RadSwDifTranGrdDif             ,& ! out,   down diffuse flux below veg (per unit dif flux)
              RadSwDirTranGrdDif            => noahmp%energy%flux%RadSwDirTranGrdDif             ,& ! out,   down direct flux below veg per unit dif flux (= 0)
              RadSwReflVegDir           => noahmp%energy%flux%RadSwReflVegDir            ,& ! out,   flux reflected by veg layer (per unit direct flux)
              RadSwReflVegDif           => noahmp%energy%flux%RadSwReflVegDif            ,& ! out,   flux reflected by veg layer (per unit diffuse flux)
              RadSwReflGrdDir           => noahmp%energy%flux%RadSwReflGrdDir            ,& ! out,   flux reflected by ground (per unit direct flux)
              RadSwReflGrdDif           => noahmp%energy%flux%RadSwReflGrdDif             & ! out,   flux reflected by ground (per unit diffuse flux)
             )
! ----------------------------------------------------------------------

    ! initialization
    MPE   = 1.0e-06
    BGAP  = 0.0
    WGAP  = 0.0
    GDIR  = 0.0
    RHO   = 0.0
    TAU   = 0.0
    FSUN  = 0.0
    do IB = 1, NumSWRadBand
       ALBD(IB)   = 0.0
       ALBI(IB)   = 0.0
       ALBGRD(IB) = 0.0
       ALBGRI(IB) = 0.0
       ALBSND(IB) = 0.0
       ALBSNI(IB) = 0.0
       RadSwAbsVegDir(IB)   = 0.0
       RadSwAbsVegDif(IB)   = 0.0
       RadSwDirTranGrdDir(IB)   = 0.0
       RadSwDirTranGrdDif(IB)   = 0.0
       RadSwDifTranGrdDir(IB)   = 0.0
       RadSwDifTranGrdDif(IB)   = 0.0
       RadSwReflVegDir(IB)  = 0.0
       RadSwReflVegDif(IB)  = 0.0
       RadSwReflGrdDir(IB)  = 0.0
       RadSwReflGrdDif(IB)  = 0.0
    enddo
    VAI = ELAI + ESAI

    ! solar radiation process is only done if there is light
    if ( CosSolarZenithAngle > 0 ) then

       ! weight reflectance/transmittance by LAI and SAI
       WL  = ELAI / max(VAI, MPE)
       WS  = ESAI / max(VAI, MPE)
       do IB = 1, NumSWRadBand
          RHO(IB) = max( RHOL(IB)*WL + RHOS(IB)*WS, MPE )
          TAU(IB) = max( TAUL(IB)*WL + TAUS(IB)*WS, MPE )
       enddo

       ! snow aging
       call SnowAgingBats(noahmp)

       ! snow albedos
       if ( OptSnowAlbedo == 1 )  call SnowAlbedoBats(noahmp)
       if ( OptSnowAlbedo == 2 )  call SnowAlbedoClass(noahmp)

       ! ground surface albedo
       call GroundAlbedo(noahmp)

       ! loop over shortwave bands to calculate surface albedos and solar
       ! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)
       do IB = 1, NumSWRadBand
          IC = 0      ! direct
          call CanopyRadiationTwoStream(noahmp, IB, IC)
          IC = 1      ! diffuse
          call CanopyRadiationTwoStream(noahmp, IB, IC)
       enddo

       ! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.
       EXT  = GDIR / CosSolarZenithAngle * sqrt( 1.0 - RHO(1) - TAU(1) )
       FSUN = ( 1.0 - exp(-EXT * VAI) ) / max( EXT*VAI, MPE )
       EXT  = FSUN
       if ( EXT < 0.01 ) then
          WL = 0.0
       else
          WL = EXT
       endif
       FSUN = WL

    endif  ! CosSolarZenithAngle > 0

    ! shaded canopy fraction
    FSHA   = 1.0 - FSUN
    LAISUN = ELAI * FSUN
    LAISHA = ELAI * FSHA

    end associate

  end subroutine SurfaceAlbedo

end module SurfaceAlbedoMod
