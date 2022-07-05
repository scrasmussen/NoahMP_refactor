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
              FSNO            => noahmp%water%state%FSNO             ,& ! in,    snow cover fraction (-)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              HVT             => noahmp%energy%param%HVT             ,& ! in,    top of canopy (m)
              Z0MVT           => noahmp%energy%param%Z0MVT           ,& ! in,    momentum roughness length vegetated (m)
              Z0SNO           => noahmp%energy%param%Z0SNO           ,& ! in,    snow surface roughness length (m)
              Z0SOIL          => noahmp%energy%param%Z0SOIL          ,& ! in,    bare-soil roughness length (m) (i.e., under the canopy)
              Z0LAKE          => noahmp%energy%param%Z0LAKE          ,& ! in,    lake surface roughness length (m)
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
          Z0MG = Z0LAKE * (1.0 - FSNO) + FSNO * Z0SNO
       else
          Z0MG = Z0LAKE
       endif
    else  ! soil
       Z0MG = Z0SOIL * (1.0 - FSNO) + FSNO * Z0SNO
    endif

    ! surface roughness length and displacement height
    ZPDG = SNOWH
    if ( VEG .eqv. .true. ) then
       Z0M = Z0MVT
       ZPD = 0.65 * HVT
       if ( SNOWH > ZPD ) ZPD = SNOWH
    else
       Z0M = Z0MG
       ZPD = ZPDG
    endif

    ! special case for urban
    if ( FlagUrban .eqv. .true. ) then
       Z0MG = Z0MVT
       ZPDG = 0.65 * HVT
       Z0M  = Z0MG
       ZPD  = ZPDG
    endif

    ! reference height above ground
    RefHeightAboveGround = max( ZPD, HVT ) + RefHeightAboveSfc
    if ( ZPDG >= RefHeightAboveGround ) RefHeightAboveGround = ZPDG + RefHeightAboveSfc

    end associate

  end subroutine GroundRoughnessProperty

end module GroundRoughnessPropertyMod
