module SnowAlbedoBatsMod

!!! Compute snow albedo based on BATS scheme (Yang et al. (1997) J.of Climate)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowAlbedoBats(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWALB_BATS
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB          ! waveband class
    real(kind=kind_noahmp)           :: FZEN        ! zenith angle correction
    real(kind=kind_noahmp)           :: CF1         ! temperary variable
    real(kind=kind_noahmp)           :: SL2         ! 2.*SL
    real(kind=kind_noahmp)           :: SL1         ! 1/SL
    real(kind=kind_noahmp)           :: SL          ! adjustable parameter

! --------------------------------------------------------------------
    associate(                                                        &
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,     number of solar radiation wave bands
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              SolarZenithAdjBats       => noahmp%energy%param%SolarZenithAdjBats       ,& ! in,     zenith angle snow albedo adjustment
              FreshSnoAlbVisBats    => noahmp%energy%param%FreshSnoAlbVisBats    ,& ! in,     new snow visible albedo
              FreshSnoAlbNirBats    => noahmp%energy%param%FreshSnoAlbNirBats    ,& ! in,     new snow NIR albedo
              SnoAgeFacDifVisBats    => noahmp%energy%param%SnoAgeFacDifVisBats    ,& ! in,     age factor for diffuse visible snow albedo
              SnoAgeFacDifNirBats    => noahmp%energy%param%SnoAgeFacDifNirBats    ,& ! in,     age factor for diffuse NIR snow albedo
              SzaFacDirVisBats    => noahmp%energy%param%SzaFacDirVisBats    ,& ! in,     cosz factor for direct visible snow albedo
              SzaFacDirNirBats    => noahmp%energy%param%SzaFacDirNirBats    ,& ! in,     cosz factor for direct NIR snow albedo
              SnowAgeFac            => noahmp%energy%state%SnowAgeFac            ,& ! in,     snow age factor
              AlbedoSnowDir          => noahmp%energy%state%AlbedoSnowDir          ,& ! out,    snow albedo for direct(1=vis, 2=nir)
              AlbedoSnowDif          => noahmp%energy%state%AlbedoSnowDif           & ! out,    snow albedo for diffuse(1=vis, 2=nir)
             )
! ----------------------------------------------------------------------

    ! initialization
    AlbedoSnowDir(1: NumSWRadBand) = 0.0
    AlbedoSnowDif(1: NumSWRadBand) = 0.0

    ! when CosSolarZenithAngle > 0
    SL        = SolarZenithAdjBats
    SL1       = 1.0 / SL
    SL2       = 2.0 * SL
    CF1       = ( (1.0 + SL1) / (1.0 + SL2*CosSolarZenithAngle) - SL1 )
    FZEN      = amax1( CF1, 0.0 )
    AlbedoSnowDif(1) = FreshSnoAlbVisBats * ( 1.0 - SnoAgeFacDifVisBats * SnowAgeFac )           ! vis diffuse
    AlbedoSnowDif(2) = FreshSnoAlbNirBats * ( 1.0 - SnoAgeFacDifNirBats * SnowAgeFac )           ! nir diffuse
    AlbedoSnowDir(1) = AlbedoSnowDif(1) + SzaFacDirVisBats * FZEN * (1.0 - AlbedoSnowDif(1))    ! vis direct
    AlbedoSnowDir(2) = AlbedoSnowDif(2) + SzaFacDirNirBats * FZEN * (1.0 - AlbedoSnowDif(2))    ! nir direct

    end associate

  end subroutine SnowAlbedoBats

end module SnowAlbedoBatsMod
