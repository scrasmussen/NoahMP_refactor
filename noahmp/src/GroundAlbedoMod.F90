module GroundAlbedoMod

!!! Compute ground albedo based on soil and snow albedo

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundAlbedo(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROUNDALB
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB      ! waveband class
    real(kind=kind_noahmp)           :: INC     ! soil water correction factor for soil albedo

! --------------------------------------------------------------------
    associate(                                                        &
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,     number of solar radiation wave bands
              IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake
              COSZ            => noahmp%config%domain%COSZ           ,& ! in,     cosine solar zenith angle
              FSNO            => noahmp%water%state%FSNO             ,& ! in,     snow cover fraction (-)
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              ALBSAT          => noahmp%energy%param%ALBSAT          ,& ! in,     saturated soil albedos: 1=vis, 2=nir
              ALBDRY          => noahmp%energy%param%ALBDRY          ,& ! in,     dry soil albedos: 1=vis, 2=nir
              ALBLAK          => noahmp%energy%param%ALBLAK          ,& ! in,     albedo frozen lakes: 1=vis, 2=nir
              TG              => noahmp%energy%state%TG              ,& ! in,     ground temperature (k)
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! in,     snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! in,     snow albedo for diffuse(1=vis, 2=nir)
              ALBSOD          => noahmp%energy%state%ALBSOD          ,& ! out,    soil albedo (direct)
              ALBSOI          => noahmp%energy%state%ALBSOI          ,& ! out,    soil albedo (diffuse)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! out,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI           & ! out,    ground albedo (diffuse: vis, nir)
             )
! ----------------------------------------------------------------------

    do IB = 1, NBAND

       INC = max( 0.11 - 0.40*SMC(1), 0.0 )

       if ( IST == 1 )  then  ! soil
          ALBSOD(IB) = min( ALBSAT(IB)+INC, ALBDRY(IB) )
          ALBSOI(IB) = ALBSOD(IB)
       elseif ( TG > ConstFreezePoint ) then  ! unfrozen lake, wetland
          ALBSOD(IB) = 0.06 / ( max(0.01, COSZ)**1.7 + 0.15 )
          ALBSOI(IB) = 0.06
       else    !frozen lake, wetland
          ALBSOD(IB) = ALBLAK(IB)
          ALBSOI(IB) = ALBSOD(IB)
       endif

       ALBGRD(IB) = ALBSOD(IB) * (1.0 - FSNO) + ALBSND(IB) * FSNO
       ALBGRI(IB) = ALBSOI(IB) * (1.0 - FSNO) + ALBSNI(IB) * FSNO

    enddo

    end associate

  end subroutine GroundAlbedo

end module GroundAlbedoMod
