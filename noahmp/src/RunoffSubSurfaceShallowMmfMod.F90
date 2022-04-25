module RunoffSubSurfaceShallowMmfMod

!!! Calculate subsurface runoff based on TOPMODEL with groundwater (Niu et al 2007)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use ShallowWaterTableMmfMod, only : ShallowWaterTableMMF

  implicit none

contains

  subroutine RunoffSubSurfaceShallowWaterMMF(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in WATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! in,     soil bottom drainage (m/s)
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil water content [m3/m3]
              WA              => noahmp%water%state%WA               ,& ! inout,  water storage in aquifer [mm]
              RUNSUB          => noahmp%water%flux%RUNSUB             & ! out,   subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! compute shallow water table and moisture
    call ShallowWaterTableMMF(noahmp)

    ! update moisture
    SH2O(NSOIL) = SMC(NSOIL) - SICE(NSOIL)

    ! compute subsurface runoff
    ! it really comes from subroutine watertable, which is not called with the same frequency as the soil routines here
    RUNSUB = RUNSUB + QDRAIN 
    WA = 0.0

    end associate

  end subroutine RunoffSubSurfaceShallowWaterMMF

end module RunoffSubSurfaceShallowMmfMod
