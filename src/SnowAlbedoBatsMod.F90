module SnowAlbedoBatsMod

!!! Compute snow albedo based on BATS scheme (Yang et al. (1997) J.of Climate)

  use Machine, only : kind_noahmp
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
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,     number of solar radiation wave bands
              COSZ            => noahmp%config%domain%COSZ           ,& ! in,     cosine solar zenith angle
              FSNO            => noahmp%water%state%FSNO             ,& ! in,     snow cover fraction (-)
              BATS_COSZ       => noahmp%energy%param%BATS_COSZ       ,& ! in,     zenith angle snow albedo adjustment
              BATS_VIS_NEW    => noahmp%energy%param%BATS_VIS_NEW    ,& ! in,     new snow visible albedo
              BATS_NIR_NEW    => noahmp%energy%param%BATS_NIR_NEW    ,& ! in,     new snow NIR albedo
              BATS_VIS_AGE    => noahmp%energy%param%BATS_VIS_AGE    ,& ! in,     age factor for diffuse visible snow albedo
              BATS_NIR_AGE    => noahmp%energy%param%BATS_NIR_AGE    ,& ! in,     age factor for diffuse NIR snow albedo
              BATS_VIS_DIR    => noahmp%energy%param%BATS_VIS_DIR    ,& ! in,     cosz factor for direct visible snow albedo
              BATS_NIR_DIR    => noahmp%energy%param%BATS_NIR_DIR    ,& ! in,     cosz factor for direct NIR snow albedo
              FAGE            => noahmp%energy%state%FAGE            ,& ! in,     snow age factor
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! out,    snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI           & ! out,    snow albedo for diffuse(1=vis, 2=nir)
             )
! ----------------------------------------------------------------------

    ! initialization
    ALBSND(1: NBAND) = 0.0
    ALBSNI(1: NBAND) = 0.0

    ! when cosz > 0
    SL        = BATS_COSZ
    SL1       = 1.0 / SL
    SL2       = 2.0 * SL
    CF1       = ( (1.0 + SL1) / (1.0 + SL2*COSZ) - SL1 )
    FZEN      = amax1( CF1, 0.0 )
    ALBSNI(1) = BATS_VIS_NEW * ( 1.0 - BATS_VIS_AGE * FAGE )           ! vis diffuse
    ALBSNI(2) = BATS_NIR_NEW * ( 1.0 - BATS_NIR_AGE * FAGE )           ! nir diffuse
    ALBSND(1) = ALBSNI(1) + BATS_VIS_DIR * FZEN * (1.0 - ALBSNI(1))    ! vis direct
    ALBSND(2) = ALBSNI(2) + BATS_NIR_DIR * FZEN * (1.0 - ALBSNI(2))    ! nir direct

    end associate

  end subroutine SnowAlbedoBats

end module SnowAlbedoBatsMod
