module HumiditySaturationMod

!!! Compute saturated surface specific humidity and changing rate to temperature

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine HumiditySaturation(AIRTMP, AIRPRS, Q2SAT, DQSDT2)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CALHUM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    real(kind=kind_noahmp), intent(in)   :: AIRTMP    ! air temperature (K)
    real(kind=kind_noahmp), intent(in)   :: AIRPRS    ! air pressure (pa)
    real(kind=kind_noahmp), intent(out)  :: Q2SAT     ! saturation specific humidity (g/g)
    real(kind=kind_noahmp), intent(out)  :: DQSDT2    ! d(Q2SAT)/d(T)

! local variable
    real(kind=kind_noahmp), parameter    :: A2       = 17.67
    real(kind=kind_noahmp), parameter    :: A3       = 273.15
    real(kind=kind_noahmp), parameter    :: A4       = 29.65
    real(kind=kind_noahmp), parameter    :: ELWV     = 2.501e6
    real(kind=kind_noahmp), parameter    :: A23M4    = A2*(A3-A4)
    real(kind=kind_noahmp), parameter    :: E0       = 0.611
    real(kind=kind_noahmp), parameter    :: RV       = 461.0
    real(kind=kind_noahmp), parameter    :: EPSILON0 = 0.622
    real(kind=kind_noahmp)               :: ES, SFCPRSX  ! temporary vars

! ----------------------------------------------------------------------

    ! Q2SAT: saturated mixing ratio
    ES = E0 * exp( ELWV / RV * (1.0/A3 - 1.0/AIRTMP) )

    ! convert AIRPRS from Pa to KPa
    SFCPRSX = AIRPRS * 1.0e-3
    Q2SAT   = EPSILON0 * ES / (SFCPRSX - ES)

    ! convert from  g/g to g/kg
    Q2SAT = Q2SAT * 1.0e3

    ! DQSDT2 is calculated assuming Q2SAT is a specific humidity
    DQSDT2 = ( Q2SAT / (1+Q2SAT) ) * A23M4 / (AIRTMP - A4)**2

    ! DG Q2SAT needs to be in g/g when returned for SFLX
    Q2SAT = Q2SAT / 1.0e3

  end subroutine HumiditySaturation

end module HumiditySaturationMod
