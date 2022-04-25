module SurfaceAlbedoMod

!!! Compute total surface albedo and vegetation radiative fluxes 
!!! per unit incoming direct and diffuse radiation and sunlit fraction of canopy

  use Machine, only : kind_noahmp
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
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,    number of solar radiation wave bands
              COSZ            => noahmp%config%domain%COSZ           ,& ! in,    cosine solar zenith angle
              OPT_ALB         => noahmp%config%nmlist%OPT_ALB        ,& ! in,    options for ground snow surface albedo
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
              FABD            => noahmp%energy%flux%FABD             ,& ! out,   flux abs by veg (per unit direct flux)
              FABI            => noahmp%energy%flux%FABI             ,& ! out,   flux abs by veg (per unit diffuse flux)
              FTDD            => noahmp%energy%flux%FTDD             ,& ! out,   down direct flux below veg (per unit dir flux)
              FTID            => noahmp%energy%flux%FTID             ,& ! out,   down diffuse flux below veg (per unit dir flux)
              FTII            => noahmp%energy%flux%FTII             ,& ! out,   down diffuse flux below veg (per unit dif flux)
              FTDI            => noahmp%energy%flux%FTDI             ,& ! out,   down direct flux below veg per unit dif flux (= 0)
              FREVD           => noahmp%energy%flux%FREVD            ,& ! out,   flux reflected by veg layer (per unit direct flux)
              FREVI           => noahmp%energy%flux%FREVI            ,& ! out,   flux reflected by veg layer (per unit diffuse flux)
              FREGD           => noahmp%energy%flux%FREGD            ,& ! out,   flux reflected by ground (per unit direct flux)
              FREGI           => noahmp%energy%flux%FREGI             & ! out,   flux reflected by ground (per unit diffuse flux)
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
    do IB = 1, NBAND
       ALBD(IB)   = 0.0
       ALBI(IB)   = 0.0
       ALBGRD(IB) = 0.0
       ALBGRI(IB) = 0.0
       ALBSND(IB) = 0.0
       ALBSNI(IB) = 0.0
       FABD(IB)   = 0.0
       FABI(IB)   = 0.0
       FTDD(IB)   = 0.0
       FTDI(IB)   = 0.0
       FTID(IB)   = 0.0
       FTII(IB)   = 0.0
       FREVD(IB)  = 0.0
       FREVI(IB)  = 0.0
       FREGD(IB)  = 0.0
       FREGI(IB)  = 0.0
    enddo
    VAI = ELAI + ESAI

    ! solar radiation process is only done if COSZ > 0
    if ( COSZ > 0 ) then

       ! weight reflectance/transmittance by LAI and SAI
       WL  = ELAI / max(VAI, MPE)
       WS  = ESAI / max(VAI, MPE)
       do IB = 1, NBAND
          RHO(IB) = max( RHOL(IB)*WL + RHOS(IB)*WS, MPE )
          TAU(IB) = max( TAUL(IB)*WL + TAUS(IB)*WS, MPE )
       enddo

       ! snow aging
       call SnowAgingBats(noahmp)

       ! snow albedos: only if COSZ > 0 and FSNO > 0
       if ( OPT_ALB == 1 )  call SnowAlbedoBats(noahmp)
       if ( OPT_ALB == 2 )  call SnowAlbedoClass(noahmp)

       ! ground surface albedo
       call GroundAlbedo(noahmp)

       ! loop over NBAND wavebands to calculate surface albedos and solar
       ! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)
       do IB = 1, NBAND
          IC = 0      ! direct
          call CanopyRadiationTwoStream(noahmp, IB, IC)
          IC = 1      ! diffuse
          call CanopyRadiationTwoStream(noahmp, IB, IC)
       enddo

       ! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.
       EXT  = GDIR / COSZ * sqrt( 1.0 - RHO(1) - TAU(1) )
       FSUN = ( 1.0 - exp(-EXT * VAI) ) / max( EXT*VAI, MPE )
       EXT  = FSUN
       if ( EXT < 0.01 ) then
          WL = 0.0
       else
          WL = EXT
       endif
       FSUN = WL

    endif  ! COSZ > 0

    ! shaded canopy fraction
    FSHA   = 1.0 - FSUN
    LAISUN = ELAI * FSUN
    LAISHA = ELAI * FSHA

    end associate

  end subroutine SurfaceAlbedo

end module SurfaceAlbedoMod
