module GroundAlbedoMod

!!! Compute ground albedo based on soil and snow albedo

  use Machine
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
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,     number of solar radiation wave bands
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,     surface type 1-soil; 2-lake
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,     snow cover fraction [-]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,     total soil moisture [m3/m3]
              AlbedoSoilSat          => noahmp%energy%param%AlbedoSoilSat          ,& ! in,     saturated soil albedos: 1=vis, 2=nir
              AlbedoSoilDry          => noahmp%energy%param%AlbedoSoilDry          ,& ! in,     dry soil albedos: 1=vis, 2=nir
              AlbedoLakeFrz          => noahmp%energy%param%AlbedoLakeFrz          ,& ! in,     albedo frozen lakes: 1=vis, 2=nir
              TG              => noahmp%energy%state%TG              ,& ! in,     ground temperature (k)
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! in,     snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! in,     snow albedo for diffuse(1=vis, 2=nir)
              ALBSOD          => noahmp%energy%state%ALBSOD          ,& ! out,    soil albedo (direct)
              ALBSOI          => noahmp%energy%state%ALBSOI          ,& ! out,    soil albedo (diffuse)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! out,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI           & ! out,    ground albedo (diffuse: vis, nir)
             )
! ----------------------------------------------------------------------

    do IB = 1, NumSWRadBand

       INC = max( 0.11 - 0.40*SoilMoisture(1), 0.0 )

       if ( SurfaceType == 1 )  then  ! soil
          ALBSOD(IB) = min( AlbedoSoilSat(IB)+INC, AlbedoSoilDry(IB) )
          ALBSOI(IB) = ALBSOD(IB)
       elseif ( TG > ConstFreezePoint ) then  ! unfrozen lake, wetland
          ALBSOD(IB) = 0.06 / ( max(0.01, CosSolarZenithAngle)**1.7 + 0.15 )
          ALBSOI(IB) = 0.06
       else    !frozen lake, wetland
          ALBSOD(IB) = AlbedoLakeFrz(IB)
          ALBSOI(IB) = ALBSOD(IB)
       endif

       ALBGRD(IB) = ALBSOD(IB) * (1.0 - SnowCoverFrac) + ALBSND(IB) * SnowCoverFrac
       ALBGRI(IB) = ALBSOI(IB) * (1.0 - SnowCoverFrac) + ALBSNI(IB) * SnowCoverFrac

    enddo

    end associate

  end subroutine GroundAlbedo

end module GroundAlbedoMod
