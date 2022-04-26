module GroundRoughnessPropertyGlacierMod

!!! Compute glacier ground roughness length, displacement height, and surface reference height

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundRoughnessPropertyGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              ZREF            => noahmp%config%domain%ZREF           ,& ! in,    reference height  (m)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              Z0SNO           => noahmp%energy%param%Z0SNO           ,& ! in,    snow surface roughness length (m)
              Z0M             => noahmp%energy%state%Z0M             ,& ! out,   roughness length, momentum, (m), surface
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! out,   roughness length, momentum, ground (m)
              ZPD             => noahmp%energy%state%ZPD             ,& ! out,   surface zero plane displacement (m)
              ZPDG            => noahmp%energy%state%ZPDG            ,& ! out,   ground zero plane displacement (m)
              ZLVL            => noahmp%energy%state%ZLVL             & ! out,   surface reference height  (m)
             )
! ----------------------------------------------------------------------

    ! ground roughness length
    Z0MG = Z0SNO
    Z0M = Z0MG

    ! surface roughness length and displacement height
    ZPDG = SNOWH
    ZPD = ZPDG

    ! surface reference height  (m)
    ZLVL = ZPD + ZREF

    end associate

  end subroutine GroundRoughnessPropertyGlacier

end module GroundRoughnessPropertyGlacierMod
