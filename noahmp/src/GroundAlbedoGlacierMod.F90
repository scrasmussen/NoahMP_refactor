module GroundAlbedoGlacierMod

!!! Compute glacier ground albedo based on snow and ice albedo

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundAlbedoGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB      ! waveband class

! --------------------------------------------------------------------
    associate(                                                        &
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,     number of solar radiation wave bands
              FSNO            => noahmp%water%state%FSNO             ,& ! in,     snow cover fraction (-)
              ALBICE          => noahmp%energy%param%ALBICE          ,& ! in,     albedo land ice: 1=vis, 2=nir
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! in,     snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! in,     snow albedo for diffuse(1=vis, 2=nir)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! out,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI           & ! out,    ground albedo (diffuse: vis, nir)
             )
! ----------------------------------------------------------------------

    do IB = 1, NBAND
       ALBGRD(IB) = ALBICE(IB) * (1.0 - FSNO) + ALBSND(IB) * FSNO
       ALBGRI(IB) = ALBICE(IB) * (1.0 - FSNO) + ALBSNI(IB) * FSNO
    enddo

    end associate

  end subroutine GroundAlbedoGlacier

end module GroundAlbedoGlacierMod
