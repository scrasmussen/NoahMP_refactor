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
    real(kind=kind_noahmp)           :: WL       ! fraction of LeafAreaIndex+StemAreaIndex that is LeafAreaIndex
    real(kind=kind_noahmp)           :: WS       ! fraction of LeafAreaIndex+StemAreaIndex that is StemAreaIndex
    real(kind=kind_noahmp)           :: MPE      ! prevents overflow for division by zero
    real(kind=kind_noahmp)           :: EXT      ! optical depth direct beam per unit leaf + stem area

! --------------------------------------------------------------------
    associate(                                                        &
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,    number of solar radiation wave bands
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              OptSnowAlbedo   => noahmp%config%nmlist%OptSnowAlbedo  ,& ! in,    options for ground snow surface albedo
              ReflectanceLeaf            => noahmp%energy%param%ReflectanceLeaf            ,& ! in,    leaf reflectance: 1=vis, 2=nir
              ReflectanceStem            => noahmp%energy%param%ReflectanceStem            ,& ! in,    stem reflectance: 1=vis, 2=nir
              TransmittanceLeaf            => noahmp%energy%param%TransmittanceLeaf            ,& ! in,    leaf transmittance: 1=vis, 2=nir
              TransmittanceStem            => noahmp%energy%param%TransmittanceStem            ,& ! in,    stem transmittance: 1=vis, 2=nir
              LeafAreaIndEff            => noahmp%energy%state%LeafAreaIndEff            ,& ! in,    leaf area index, after burying by snow
              StemAreaIndEff            => noahmp%energy%state%StemAreaIndEff            ,& ! in,    stem area index, after burying by snow
              AlbedoGrdDir          => noahmp%energy%state%AlbedoGrdDir          ,& ! out,   ground albedo (direct beam: vis, nir)
              AlbedoGrdDif          => noahmp%energy%state%AlbedoGrdDif          ,& ! out,   ground albedo (diffuse: vis, nir)
              AlbedoSnowDir          => noahmp%energy%state%AlbedoSnowDir          ,& ! out,   snow albedo for direct(1=vis, 2=nir)
              AlbedoSnowDif          => noahmp%energy%state%AlbedoSnowDif          ,& ! out,   snow albedo for diffuse(1=vis, 2=nir)
              AlbedoSfcDir            => noahmp%energy%state%AlbedoSfcDir            ,& ! out,   surface albedo (direct)
              AlbedoSfcDif            => noahmp%energy%state%AlbedoSfcDif            ,& ! out,   surface albedo (diffuse)
              CanopySunlitFrac            => noahmp%energy%state%CanopySunlitFrac            ,& ! out,   sunlit fraction of canopy
              CanopyShadeFrac            => noahmp%energy%state%CanopyShadeFrac            ,& ! out,   shaded fraction of canopy
              LeafAreaIndSunlit          => noahmp%energy%state%LeafAreaIndSunlit          ,& ! out,   sunlit leaf area
              LeafAreaIndShade          => noahmp%energy%state%LeafAreaIndShade          ,& ! out,   shaded leaf area
              GapBtwCanopy            => noahmp%energy%state%GapBtwCanopy            ,& ! out,   between canopy gap fraction for beam
              GapInCanopy            => noahmp%energy%state%GapInCanopy            ,& ! out,   within canopy gap fraction for beam
              ReflectanceVeg             => noahmp%energy%state%ReflectanceVeg             ,& ! out,   leaf/stem reflectance weighted by fraction LeafAreaIndex and StemAreaIndex
              TransmittanceVeg             => noahmp%energy%state%TransmittanceVeg             ,& ! out,   leaf/stem transmittance weighted by fraction LeafAreaIndex and StemAreaIndex
              VegAreaIndEff             => noahmp%energy%state%VegAreaIndEff             ,& ! out,   one-sided leaf+stem area index (m2/m2)
              VegAreaProjDir            => noahmp%energy%state%VegAreaProjDir            ,& ! out,   projected leaf+stem area in solar direction
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
    GapBtwCanopy  = 0.0
    GapInCanopy  = 0.0
    VegAreaProjDir  = 0.0
    ReflectanceVeg   = 0.0
    TransmittanceVeg   = 0.0
    CanopySunlitFrac  = 0.0
    do IB = 1, NumSWRadBand
       AlbedoSfcDir(IB)   = 0.0
       AlbedoSfcDif(IB)   = 0.0
       AlbedoGrdDir(IB) = 0.0
       AlbedoGrdDif(IB) = 0.0
       AlbedoSnowDir(IB) = 0.0
       AlbedoSnowDif(IB) = 0.0
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
    VegAreaIndEff = LeafAreaIndEff + StemAreaIndEff

    ! solar radiation process is only done if there is light
    if ( CosSolarZenithAngle > 0 ) then

       ! weight reflectance/transmittance by LeafAreaIndex and StemAreaIndex
       WL  = LeafAreaIndEff / max(VegAreaIndEff, MPE)
       WS  = StemAreaIndEff / max(VegAreaIndEff, MPE)
       do IB = 1, NumSWRadBand
          ReflectanceVeg(IB) = max( ReflectanceLeaf(IB)*WL + ReflectanceStem(IB)*WS, MPE )
          TransmittanceVeg(IB) = max( TransmittanceLeaf(IB)*WL + TransmittanceStem(IB)*WS, MPE )
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

       ! sunlit fraction of canopy. set CanopySunlitFrac = 0 if CanopySunlitFrac < 0.01.
       EXT  = VegAreaProjDir / CosSolarZenithAngle * sqrt( 1.0 - ReflectanceVeg(1) - TransmittanceVeg(1) )
       CanopySunlitFrac = ( 1.0 - exp(-EXT * VegAreaIndEff) ) / max( EXT*VegAreaIndEff, MPE )
       EXT  = CanopySunlitFrac
       if ( EXT < 0.01 ) then
          WL = 0.0
       else
          WL = EXT
       endif
       CanopySunlitFrac = WL

    endif  ! CosSolarZenithAngle > 0

    ! shaded canopy fraction
    CanopyShadeFrac   = 1.0 - CanopySunlitFrac
    LeafAreaIndSunlit = LeafAreaIndEff * CanopySunlitFrac
    LeafAreaIndShade = LeafAreaIndEff * CanopyShadeFrac

    end associate

  end subroutine SurfaceAlbedo

end module SurfaceAlbedoMod
