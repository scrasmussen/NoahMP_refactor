module SurfaceRadiationMod

!!! Compute surface (ground and vegetation) radiative fluxes (absorption and reflection)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceRadiation(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SURRAD
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB       ! waveband indices (1=vis, 2=nir)
    real(kind=kind_noahmp)           :: MPE      ! prevents overflow for division by zero
    real(kind=kind_noahmp)           :: ABSG     ! ground absorbed solar radiation (w/m2)
    real(kind=kind_noahmp)           :: RNIR     ! reflected solar radiation [nir] (w/m2)
    real(kind=kind_noahmp)           :: RVIS     ! reflected solar radiation [vis] (w/m2)
    real(kind=kind_noahmp)           :: LAIFRA   ! leaf area fraction of canopy
    real(kind=kind_noahmp)           :: TRD      ! transmitted solar radiation: direct (w/m2)
    real(kind=kind_noahmp)           :: TRI      ! transmitted solar radiation: diffuse (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:) :: CAD     ! direct beam absorbed by canopy (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:) :: CAI     ! diffuse radiation absorbed by canopy (w/m2)

! --------------------------------------------------------------------
    associate(                                                        &
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,    number of solar radiation wave bands
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              VAI             => noahmp%energy%state%VAI             ,& ! in,    one-sided leaf+stem area index (m2/m2)
              FSUN            => noahmp%energy%state%FSUN            ,& ! in,    sunlit fraction of canopy
              FSHA            => noahmp%energy%state%FSHA            ,& ! in,    shaded fraction of canopy
              LAISUN          => noahmp%energy%state%LAISUN          ,& ! in,    sunlit leaf area
              LAISHA          => noahmp%energy%state%LAISHA          ,& ! in,    shaded leaf area
              SOLAD           => noahmp%energy%flux%SOLAD            ,& ! in,    incoming direct solar radiation (w/m2)
              SOLAI           => noahmp%energy%flux%SOLAI            ,& ! in,    incoming diffuse solar radiation (w/m2)
              FABD            => noahmp%energy%flux%FABD             ,& ! in,    flux abs by veg (per unit direct flux)
              FABI            => noahmp%energy%flux%FABI             ,& ! in,    flux abs by veg (per unit diffuse flux)
              FTDD            => noahmp%energy%flux%FTDD             ,& ! in,    down direct flux below veg (per unit dir flux)
              FTID            => noahmp%energy%flux%FTID             ,& ! in,    down diffuse flux below veg (per unit dir flux)
              FTII            => noahmp%energy%flux%FTII             ,& ! in,    down diffuse flux below veg (per unit dif flux)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! in,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! in,    ground albedo (diffuse: vis, nir)
              ALBD            => noahmp%energy%state%ALBD            ,& ! in,    surface albedo (direct)
              ALBI            => noahmp%energy%state%ALBI            ,& ! in,    surface albedo (diffuse)
              FREVD           => noahmp%energy%flux%FREVD            ,& ! in,    flux reflected by veg layer (per unit direct flux)
              FREVI           => noahmp%energy%flux%FREVI            ,& ! in,    flux reflected by veg layer (per unit diffuse flux)
              FREGD           => noahmp%energy%flux%FREGD            ,& ! in,    flux reflected by ground (per unit direct flux)
              FREGI           => noahmp%energy%flux%FREGI            ,& ! in,    flux reflected by ground (per unit diffuse flux)
              PARSUN          => noahmp%energy%flux%PARSUN           ,& ! out,   average absorbed par for sunlit leaves (w/m2)
              PARSHA          => noahmp%energy%flux%PARSHA           ,& ! out,   average absorbed par for shaded leaves (w/m2)
              SAV             => noahmp%energy%flux%SAV              ,& ! out,   solar radiation absorbed by vegetation (w/m2)
              SAG             => noahmp%energy%flux%SAG              ,& ! out,   solar radiation absorbed by ground (w/m2)
              FSA             => noahmp%energy%flux%FSA              ,& ! out,   total absorbed solar radiation (w/m2)
              FSR             => noahmp%energy%flux%FSR              ,& ! out,   total reflected solar radiation (w/m2)
              FSRV            => noahmp%energy%flux%FSRV             ,& ! out,   reflected solar radiation by vegetation (w/m2)
              FSRG            => noahmp%energy%flux%FSRG              & ! out,   reflected solar radiation by ground (w/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( CAD (1:NBAND) )
    allocate( CAI (1:NBAND) )
    MPE    = 1.0e-6
    SAG    = 0.0
    SAV    = 0.0
    FSA    = 0.0
    FSR    = 0.0
    FSRV   = 0.0
    FSRG   = 0.0
    PARSUN = 0.0
    PARSHA = 0.0

    do IB = 1, NBAND
       ! absorbed by canopy
       CAD(IB) = SOLAD(IB) * FABD(IB)
       CAI(IB) = SOLAI(IB) * FABI(IB)
       SAV     = SAV + CAD(IB) + CAI(IB)
       FSA     = FSA + CAD(IB) + CAI(IB)
       ! transmitted solar fluxes incident on ground
       TRD = SOLAD(IB) * FTDD(IB)
       TRI = SOLAD(IB) * FTID(IB) + SOLAI(IB) * FTII(IB)
       ! solar radiation absorbed by ground surface
       ABSG = TRD * (1.0 - ALBGRD(IB)) + TRI * (1.0 - ALBGRI(IB))
       SAG  = SAG + ABSG
       FSA  = FSA + ABSG
    enddo

    ! partition visible canopy absorption to sunlit and shaded fractions
    ! to get average absorbed par for sunlit and shaded leaves
    LAIFRA = ELAI / max(VAI, MPE)
    if ( FSUN > 0.0 ) then
       PARSUN = ( CAD(1) + FSUN * CAI(1) ) * LAIFRA / max(LAISUN, MPE)
       PARSHA = ( FSHA * CAI(1) ) * LAIFRA / max(LAISHA, MPE)
    else
       PARSUN = 0.0
       PARSHA = ( CAD(1) + CAI(1) ) * LAIFRA / max(LAISHA, MPE)
    endif

    ! reflected solar radiation
    RVIS = ALBD(1) * SOLAD(1) + ALBI(1) * SOLAI(1)
    RNIR = ALBD(2) * SOLAD(2) + ALBI(2) * SOLAI(2)
    FSR  = RVIS + RNIR

    ! reflected solar radiation of veg. and ground (combined ground)
    FSRV = FREVD(1)*SOLAD(1) + FREVI(1)*SOLAI(1) + FREVD(2)*SOLAD(2) + FREVI(2)*SOLAI(2)
    FSRG = FREGD(1)*SOLAD(1) + FREGI(1)*SOLAI(1) + FREGD(2)*SOLAD(2) + FREGI(2)*SOLAI(2)

    end associate

  end subroutine SurfaceRadiation

end module SurfaceRadiationMod
