module SurfaceAlbedoGlacierMod

!!! Compute glacier surface albedo

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowAgingBatsMod,       only : SnowAgingBats
  use SnowAlbedoBatsMod,      only : SnowAlbedoBats
  use SnowAlbedoClassMod,     only : SnowAlbedoClass
  use GroundAlbedoGlacierMod, only : GroundAlbedoGlacier

  implicit none

contains

  subroutine SurfaceAlbedoGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB       ! waveband indices

! --------------------------------------------------------------------
    associate(                                                        &
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,    number of solar radiation wave bands
              COSZ            => noahmp%config%domain%COSZ           ,& ! in,    cosine solar zenith angle
              OptSnowAlbedo   => noahmp%config%nmlist%OptSnowAlbedo  ,& ! in,    options for ground snow surface albedo
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! out,   ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! out,   ground albedo (diffuse: vis, nir)
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! out,   snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! out,   snow albedo for diffuse(1=vis, 2=nir)
              ALBD            => noahmp%energy%state%ALBD            ,& ! out,   surface albedo (direct)
              ALBI            => noahmp%energy%state%ALBI             & ! out,   surface albedo (diffuse)
             )
! ----------------------------------------------------------------------

    ! initialization
    do IB = 1, NBAND
       ALBD(IB)   = 0.0
       ALBI(IB)   = 0.0
       ALBGRD(IB) = 0.0
       ALBGRI(IB) = 0.0
       ALBSND(IB) = 0.0
       ALBSNI(IB) = 0.0
    enddo

    ! solar radiation process is only done if COSZ > 0
    if ( COSZ > 0 ) then

       ! snow aging
       call SnowAgingBats(noahmp)

       ! snow albedos: only if COSZ > 0 and FSNO > 0
       if ( OptSnowAlbedo == 1 )  call SnowAlbedoBats(noahmp)
       if ( OptSnowAlbedo == 2 )  call SnowAlbedoClass(noahmp)

       ! ground albedo
       call GroundAlbedoGlacier(noahmp)

       ! surface albedo
       ALBD = ALBGRD
       ALBI = ALBGRI

    endif  ! COSZ > 0

    end associate

  end subroutine SurfaceAlbedoGlacier

end module SurfaceAlbedoGlacierMod
