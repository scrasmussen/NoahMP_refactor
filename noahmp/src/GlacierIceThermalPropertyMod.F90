module GlacierIceThermalPropertyMod

!!! Compute glacier ice thermal conductivity based on Noah scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GlacierIceThermalProperty(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: none (embedded in ENERGY_GLACIER)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ, IZ2       ! loop index
    real(kind=kind_noahmp) :: ZMID          ! mid-point ice layer depth

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,   thickness of snow/soil layers (m)
              CVGLAICE        => noahmp%energy%state%CVGLAICE        ,& ! out,  glacier ice layer volumetric specific heat (j/m3/k)
              TKGLAICE        => noahmp%energy%state%TKGLAICE         & ! out,  glacier ice layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    do IZ = 1, NumSoilLayer
       ZMID = 0.5 * DZSNSO(IZ)
       do IZ2 = 1, IZ-1
          ZMID = ZMID + DZSNSO(IZ2)
       enddo
       CVGLAICE(IZ) = 1.0e6 * (0.8194 + 0.1309 * ZMID)
       TKGLAICE(IZ) = 0.32333 + (0.10073 * ZMID)
    enddo

    end associate

  end subroutine GlacierIceThermalProperty

end module GlacierIceThermalPropertyMod
