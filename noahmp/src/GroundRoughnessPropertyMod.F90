module GroundRoughnessPropertyMod

!!! Compute ground roughness length, displacement height, and surface reference height

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundRoughnessProperty(noahmp, VEG)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp
    logical          , intent(in   ) :: VEG          ! true if vegetated surface

! --------------------------------------------------------------------
    associate(                                                        &
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              RefHeightAboveSfc => noahmp%config%domain%RefHeightAboveSfc ,& ! in,  reference height [m] above surface zero plane
              FlagUrban      => noahmp%config%domain%FlagUrban     ,& ! in,    logical flag for urban grid
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,    snow cover fraction [-]
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! in,    snow depth [m]
              HeightCanopyTop             => noahmp%energy%param%HeightCanopyTop             ,& ! in,    top of canopy (m)
              RoughLenMomVeg           => noahmp%energy%param%RoughLenMomVeg           ,& ! in,    momentum roughness length vegetated (m)
              RoughLenMomSno           => noahmp%energy%param%RoughLenMomSno           ,& ! in,    snow surface roughness length (m)
              RoughLenMomSoil          => noahmp%energy%param%RoughLenMomSoil          ,& ! in,    bare-soil roughness length (m) (i.e., under the canopy)
              RoughLenMomLake          => noahmp%energy%param%RoughLenMomLake          ,& ! in,    lake surface roughness length (m)
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (K)
              Z0M             => noahmp%energy%state%Z0M             ,& ! out,   roughness length, momentum, (m), surface
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! out,   roughness length, momentum, ground (m)
              ZPD             => noahmp%energy%state%ZPD             ,& ! out,   surface zero plane displacement (m)
              ZPDG            => noahmp%energy%state%ZPDG            ,& ! out,   ground zero plane displacement (m)
              RefHeightAboveGround            => noahmp%energy%state%RefHeightAboveGround  & ! out,  reference height [m] above ground
             )
! ----------------------------------------------------------------------

    ! ground roughness length
    if ( SurfaceType == 2 ) then ! Lake 
       if ( TG <= ConstFreezePoint ) then
          Z0MG = RoughLenMomLake * (1.0 - SnowCoverFrac) + SnowCoverFrac * RoughLenMomSno
       else
          Z0MG = RoughLenMomLake
       endif
    else  ! soil
       Z0MG = RoughLenMomSoil * (1.0 - SnowCoverFrac) + SnowCoverFrac * RoughLenMomSno
    endif

    ! surface roughness length and displacement height
    ZPDG = SnowDepth
    if ( VEG .eqv. .true. ) then
       Z0M = RoughLenMomVeg
       ZPD = 0.65 * HeightCanopyTop
       if ( SnowDepth > ZPD ) ZPD = SnowDepth
    else
       Z0M = Z0MG
       ZPD = ZPDG
    endif

    ! special case for urban
    if ( FlagUrban .eqv. .true. ) then
       Z0MG = RoughLenMomVeg
       ZPDG = 0.65 * HeightCanopyTop
       Z0M  = Z0MG
       ZPD  = ZPDG
    endif

    ! reference height above ground
    RefHeightAboveGround = max( ZPD, HeightCanopyTop ) + RefHeightAboveSfc
    if ( ZPDG >= RefHeightAboveGround ) RefHeightAboveGround = ZPDG + RefHeightAboveSfc

    end associate

  end subroutine GroundRoughnessProperty

end module GroundRoughnessPropertyMod
