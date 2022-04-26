module SnowLayerWaterComboMod

!!! Update snow water and temperature for combined snowpack layer

  use Machine, only : kind_noahmp
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowLayerWaterCombo(DZ, WLIQ, WICE, T, DZ2, WLIQ2, WICE2, T2)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMBO
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

! IN and OUT variables
    real(kind=kind_noahmp), intent(in)    :: DZ2      ! nodal thickness of 2 elements being combined [m]
    real(kind=kind_noahmp), intent(in)    :: WLIQ2    ! liquid water of element 2 [kg/m2]
    real(kind=kind_noahmp), intent(in)    :: WICE2    ! ice of element 2 [kg/m2]
    real(kind=kind_noahmp), intent(in)    :: T2       ! nodal temperature of element 2 [k]
    real(kind=kind_noahmp), intent(inout) :: DZ       ! nodal thickness of 1 elements being combined [m]
    real(kind=kind_noahmp), intent(inout) :: WLIQ     ! liquid water of element 1
    real(kind=kind_noahmp), intent(inout) :: WICE     ! ice of element 1 [kg/m2]
    real(kind=kind_noahmp), intent(inout) :: T        ! node temperature of element 1 [k]

! local variable
    real(kind=kind_noahmp)                :: DZC      ! total thickness of nodes 1 and 2 (DZC=DZ+DZ2)
    real(kind=kind_noahmp)                :: WLIQC    ! combined liquid water [kg/m2]
    real(kind=kind_noahmp)                :: WICEC    ! combined ice [kg/m2]
    real(kind=kind_noahmp)                :: TC       ! combined node temperature [k]
    real(kind=kind_noahmp)                :: H        ! enthalpy of element 1 [J/m2]
    real(kind=kind_noahmp)                :: H2       ! enthalpy of element 2 [J/m2]
    real(kind=kind_noahmp)                :: HC       ! temporary

! ----------------------------------------------------------------------

    DZC   = DZ + DZ2
    WICEC = WICE + WICE2
    WLIQC = WLIQ + WLIQ2
    H     = (CICE*WICE  + CWAT*WLIQ)  * (T  - TFRZ) + HFUS * WLIQ
    H2    = (CICE*WICE2 + CWAT*WLIQ2) * (T2 - TFRZ) + HFUS * WLIQ2

    HC = H + H2
    if ( HC < 0.0 ) then
       TC = TFRZ + HC / (CICE*WICEC + CWAT*WLIQC)
    else if ( HC <= HFUS*WLIQC ) then
       TC = TFRZ
    else
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    endif

    DZ   = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T    = TC

  end subroutine SnowLayerWaterCombo

end module SnowLayerWaterComboMod
