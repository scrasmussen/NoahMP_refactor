module SnowLayerWaterComboMod

!!! Update snow water and temperature for combined snowpack layer

  use Machine, only : kind_noahmp
  use NoahmpType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowLayerWaterCombo(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMBO
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp) :: DZC       ! total thickness of nodes 1 and 2 (DZC=DZ+DZ2)
    real(kind=kind_noahmp) :: WLIQC     ! combined liquid water [kg/m2]
    real(kind=kind_noahmp) :: WICEC     ! combined ice [kg/m2]
    real(kind=kind_noahmp) :: TC        ! combined node temperature [k]
    real(kind=kind_noahmp) :: H         ! enthalpy of element 1 [J/m2]
    real(kind=kind_noahmp) :: H2        ! enthalpy of element 2 [J/m2]
    real(kind=kind_noahmp) :: HC        ! temporary

! ----------------------------------------------------------------------
    associate(                                                        &
              DZ2       => noahmp%water%diag%DZ2_COMBO               ,& ! in,    nodal thickness of 2 elements being combined [m]
              WLIQ2     => noahmp%water%diag%WLIQ2_COMBO             ,& ! in,    liquid water of element 2 [kg/m2]
              WICE2     => noahmp%water%diag%WICE2_COMBO             ,& ! in,    ice of element 2 [kg/m2]
              T2        => noahmp%water%diag%T2_COMBO                ,& ! in,    nodal temperature of element 2 [k]
              DZ        => noahmp%water%diag%DZ_COMBO                ,& ! inout, nodal thickness of 1 elements being combined [m]
              WLIQ      => noahmp%water%diag%WLIQ_COMBO              ,& ! inout, liquid water of element 1
              WICE      => noahmp%water%diag%WICE_COMBO              ,& ! inout, ice of element 1 [kg/m2]
              T         => noahmp%water%diag%T_COMBO                  & ! inout, node temperature of element 1 [k]
             )
! ----------------------------------------------------------------------

    DZC   = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H     = (CICE*WICE + CWAT*WLIQ) * (T-TFRZ) + HFUS*WLIQ
    H2    = (CICE*WICE2 + CWAT*WLIQ2) * (T2-TFRZ) + HFUS*WLIQ2

    HC = H + H2
    if ( HC < 0.0 ) then
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    else if ( HC <= HFUS*WLIQC ) then
       TC = TFRZ
    else
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    endif

    DZ   = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T    = TC

    end associate

  end subroutine SnowLayerWaterCombo

end module SnowLayerWaterComboMod
