module RunoffSurfaceTopModelGrdMod

!!! Calculate surface runoff based on TOPMODEL with groundwater scheme (Niu et al., 2007)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceTopModelGrd(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              FFF             => noahmp%water%param%FFF              ,& ! in,     runoff decay factor (m-1)
              FSATMX          => noahmp%water%param%FSATMX           ,& ! in,     maximum surface saturated fraction (global mean)
              FCR             => noahmp%water%state%FCR              ,& ! in,     impermeable fraction due to frozen soil
              ZWT             => noahmp%water%state%ZWT              ,& ! in,     water table depth [m]
              FSAT            => noahmp%water%state%FSAT             ,& ! out,    fractional saturated area for soil moisture
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              PDDUM           => noahmp%water%flux%PDDUM              & ! out,    infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! set up key parameter
    FFF = 6.0

    ! compute saturated area fraction
    FSAT = FSATMX * exp( -0.5 * FFF * (ZWT-2.0) )

    ! compute surface runoff and infiltration  m/s
    if ( QINSUR > 0.0 ) then
       RUNSRF = QINSUR * ( (1.0-FCR(1)) * FSAT + FCR(1) )
       PDDUM  = QINSUR - RUNSRF 
    endif

    end associate

  end subroutine RunoffSurfaceTopModelGrd

end module RunoffSurfaceTopModelGrdMod
