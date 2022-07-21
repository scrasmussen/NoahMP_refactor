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
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,    number of solar radiation wave bands
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              VAI             => noahmp%energy%state%VAI             ,& ! in,    one-sided leaf+stem area index (m2/m2)
              FSUN            => noahmp%energy%state%FSUN            ,& ! in,    sunlit fraction of canopy
              FSHA            => noahmp%energy%state%FSHA            ,& ! in,    shaded fraction of canopy
              LAISUN          => noahmp%energy%state%LAISUN          ,& ! in,    sunlit leaf area
              LAISHA          => noahmp%energy%state%LAISHA          ,& ! in,    shaded leaf area
              RadSwDownDir           => noahmp%energy%flux%RadSwDownDir            ,& ! in,    incoming direct solar radiation (w/m2)
              RadSwDownDif           => noahmp%energy%flux%RadSwDownDif            ,& ! in,    incoming diffuse solar radiation (w/m2)
              RadSwAbsVegDir            => noahmp%energy%flux%RadSwAbsVegDir             ,& ! in,    flux abs by veg (per unit direct flux)
              RadSwAbsVegDif            => noahmp%energy%flux%RadSwAbsVegDif             ,& ! in,    flux abs by veg (per unit diffuse flux)
              RadSwDirTranGrdDir            => noahmp%energy%flux%RadSwDirTranGrdDir             ,& ! in,    down direct flux below veg (per unit dir flux)
              RadSwDifTranGrdDir            => noahmp%energy%flux%RadSwDifTranGrdDir             ,& ! in,    down diffuse flux below veg (per unit dir flux)
              RadSwDifTranGrdDif            => noahmp%energy%flux%RadSwDifTranGrdDif             ,& ! in,    down diffuse flux below veg (per unit dif flux)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! in,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! in,    ground albedo (diffuse: vis, nir)
              ALBD            => noahmp%energy%state%ALBD            ,& ! in,    surface albedo (direct)
              ALBI            => noahmp%energy%state%ALBI            ,& ! in,    surface albedo (diffuse)
              RadSwReflVegDir           => noahmp%energy%flux%RadSwReflVegDir            ,& ! in,    flux reflected by veg layer (per unit direct flux)
              RadSwReflVegDif           => noahmp%energy%flux%RadSwReflVegDif            ,& ! in,    flux reflected by veg layer (per unit diffuse flux)
              RadSwReflGrdDir           => noahmp%energy%flux%RadSwReflGrdDir            ,& ! in,    flux reflected by ground (per unit direct flux)
              RadSwReflGrdDif           => noahmp%energy%flux%RadSwReflGrdDif            ,& ! in,    flux reflected by ground (per unit diffuse flux)
              RadPhotoActAbsSunlit          => noahmp%energy%flux%RadPhotoActAbsSunlit           ,& ! out,   average absorbed par for sunlit leaves (w/m2)
              RadPhotoActAbsShade          => noahmp%energy%flux%RadPhotoActAbsShade           ,& ! out,   average absorbed par for shaded leaves (w/m2)
              RadSwAbsVeg             => noahmp%energy%flux%RadSwAbsVeg              ,& ! out,   solar radiation absorbed by vegetation (w/m2)
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! out,   solar radiation absorbed by ground (w/m2)
              RadSwAbsTot             => noahmp%energy%flux%RadSwAbsTot              ,& ! out,   total absorbed solar radiation (w/m2)
              RadSwReflTot             => noahmp%energy%flux%RadSwReflTot              ,& ! out,   total reflected solar radiation (w/m2)
              RadSwReflVeg            => noahmp%energy%flux%RadSwReflVeg             ,& ! out,   reflected solar radiation by vegetation (w/m2)
              RadSwReflGrd            => noahmp%energy%flux%RadSwReflGrd              & ! out,   reflected solar radiation by ground (w/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( CAD (1:NumSWRadBand) )
    allocate( CAI (1:NumSWRadBand) )
    MPE    = 1.0e-6
    RadSwAbsGrd    = 0.0
    RadSwAbsVeg    = 0.0
    RadSwAbsTot    = 0.0
    RadSwReflTot    = 0.0
    RadSwReflVeg   = 0.0
    RadSwReflGrd   = 0.0
    RadPhotoActAbsSunlit = 0.0
    RadPhotoActAbsShade = 0.0

    do IB = 1, NumSWRadBand
       ! absorbed by canopy
       CAD(IB) = RadSwDownDir(IB) * RadSwAbsVegDir(IB)
       CAI(IB) = RadSwDownDif(IB) * RadSwAbsVegDif(IB)
       RadSwAbsVeg     = RadSwAbsVeg + CAD(IB) + CAI(IB)
       RadSwAbsTot     = RadSwAbsTot + CAD(IB) + CAI(IB)
       ! transmitted solar fluxes incident on ground
       TRD = RadSwDownDir(IB) * RadSwDirTranGrdDir(IB)
       TRI = RadSwDownDir(IB) * RadSwDifTranGrdDir(IB) + RadSwDownDif(IB) * RadSwDifTranGrdDif(IB)
       ! solar radiation absorbed by ground surface
       ABSG = TRD * (1.0 - ALBGRD(IB)) + TRI * (1.0 - ALBGRI(IB))
       RadSwAbsGrd  = RadSwAbsGrd + ABSG
       RadSwAbsTot  = RadSwAbsTot + ABSG
    enddo

    ! partition visible canopy absorption to sunlit and shaded fractions
    ! to get average absorbed par for sunlit and shaded leaves
    LAIFRA = ELAI / max(VAI, MPE)
    if ( FSUN > 0.0 ) then
       RadPhotoActAbsSunlit = ( CAD(1) + FSUN * CAI(1) ) * LAIFRA / max(LAISUN, MPE)
       RadPhotoActAbsShade = ( FSHA * CAI(1) ) * LAIFRA / max(LAISHA, MPE)
    else
       RadPhotoActAbsSunlit = 0.0
       RadPhotoActAbsShade = ( CAD(1) + CAI(1) ) * LAIFRA / max(LAISHA, MPE)
    endif

    ! reflected solar radiation
    RVIS = ALBD(1) * RadSwDownDir(1) + ALBI(1) * RadSwDownDif(1)
    RNIR = ALBD(2) * RadSwDownDir(2) + ALBI(2) * RadSwDownDif(2)
    RadSwReflTot  = RVIS + RNIR

    ! reflected solar radiation of veg. and ground (combined ground)
    RadSwReflVeg = RadSwReflVegDir(1)*RadSwDownDir(1) + RadSwReflVegDif(1)*RadSwDownDif(1) + &
                   RadSwReflVegDir(2)*RadSwDownDir(2) + RadSwReflVegDif(2)*RadSwDownDif(2)
    RadSwReflGrd = RadSwReflGrdDir(1)*RadSwDownDir(1) + RadSwReflGrdDif(1)*RadSwDownDif(1) + &
                   RadSwReflGrdDir(2)*RadSwDownDir(2) + RadSwReflGrdDif(2)*RadSwDownDif(2)

    end associate

  end subroutine SurfaceRadiation

end module SurfaceRadiationMod
