module RunoffSubSurfaceEquiWaterTableMod

!!! Calculate subsurface runoff using equilibrium water table depth (Niu et al., 2005)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use WaterTableEquilibriumMod, only : WaterTableEquilibrium

  implicit none

contains

  subroutine RunoffSubSurfaceEquiWaterTable(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              SoilImpervFracMax          => noahmp%water%state%SoilImpervFracMax           ,& ! in,    maximum soil imperviousness fraction
              TIMEAN          => noahmp%water%param%TIMEAN           ,& ! in,     gridcell mean topgraphic index (global mean)
              FFF             => noahmp%water%param%FFF              ,& ! in,     runoff decay factor (m-1)
              RSBMX           => noahmp%water%param%RSBMX            ,& ! in,     baseflow coefficient [mm/s]
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! out,    water table depth [m]
              RUNSUB          => noahmp%water%flux%RUNSUB             & ! out,    subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! set parameter values specific for this scheme
    FFF   = 2.0
    RSBMX = 4.0

    ! compute equilibrium water table depth
    call WaterTableEquilibrium(noahmp)

    ! compuate subsurface runoff mm/s
    RUNSUB = (1.0 - SoilImpervFracMax) * RSBMX * exp(-TIMEAN) * exp(-FFF * WaterTableDepth)

    end associate

  end subroutine RunoffSubSurfaceEquiWaterTable

end module RunoffSubSurfaceEquiWaterTableMod
