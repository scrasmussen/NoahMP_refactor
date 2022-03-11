module ForcingVarType

!!! Define column (1-D) Noah-MP forcing variables
!!! Forcing variable initialization is done in ForcingVarInit.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

  type, public :: forcing_type

    ! define specific forcing variables
    real(kind=kind_noahmp) :: SFCTMP          ! surface air temperature [k]
    real(kind=kind_noahmp) :: UU              ! wind speed in eastward dir [m/s]
    real(kind=kind_noahmp) :: VV              ! wind speed in northward dir [m/s]
    real(kind=kind_noahmp) :: Q2              ! 2-m specific humidity (kg/kg)
    real(kind=kind_noahmp) :: SFCPRS          ! surface pressure (pa)
    real(kind=kind_noahmp) :: LWDN            ! downward longwave radiation [w/m2]
    real(kind=kind_noahmp) :: PSFC            ! pressure at lowest model layer (pa)
    real(kind=kind_noahmp) :: SOLDN           ! downward shortwave radiation [w/m2]
    real(kind=kind_noahmp) :: PRCPCONV        ! convective precipitation entering  [mm/s]
    real(kind=kind_noahmp) :: PRCPNONC        ! non-convective precipitation entering [mm/s]
    real(kind=kind_noahmp) :: PRCPSHCV        ! shallow convective precip entering  [mm/s]
    real(kind=kind_noahmp) :: PRCPSNOW        ! snow entering land model [mm/s]
    real(kind=kind_noahmp) :: PRCPGRPL        ! graupel entering land model [mm/s] 
    real(kind=kind_noahmp) :: PRCPHAIL        ! hail entering land model [mm/s]  
    real(kind=kind_noahmp) :: TBOT            ! bottom condition for soil temp. [K]

  end type forcing_type

end module ForcingVarType
