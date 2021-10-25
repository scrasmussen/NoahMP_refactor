module ConstantDefineMod

!!! Define Noah-MP constant variables

  use Machine, only : kind_noahmp

  implicit none
  save
  private

    ! define specific physical constants
  real(kind=kind_noahmp), public, parameter :: GRAV   = 9.80616   !acceleration due to gravity (m/s2)
  real(kind=kind_noahmp), public, parameter :: SB     = 5.67E-08  !Stefan-Boltzmann constant (w/m2/k4)
  real(kind=kind_noahmp), public, parameter :: VKC    = 0.40      !von Karman constant
  real(kind=kind_noahmp), public, parameter :: TFRZ   = 273.16    !freezing/melting point (k)
  real(kind=kind_noahmp), public, parameter :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  real(kind=kind_noahmp), public, parameter :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg)
  real(kind=kind_noahmp), public, parameter :: HFUS   = 0.3336E06 !latent heat of fusion (j/kg)
  real(kind=kind_noahmp), public, parameter :: CWAT   = 4.188E06  !specific heat capacity of water (j/m3/k)
  real(kind=kind_noahmp), public, parameter :: CICE   = 2.094E06  !specific heat capacity of ice (j/m3/k)
  real(kind=kind_noahmp), public, parameter :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  real(kind=kind_noahmp), public, parameter :: TKWAT  = 0.6       !thermal conductivity of water (w/m/k)
  real(kind=kind_noahmp), public, parameter :: TKICE  = 2.2       !thermal conductivity of ice (w/m/k)
  real(kind=kind_noahmp), public, parameter :: TKAIR  = 0.023     !thermal conductivity of air (w/m/k) (not used MB: 20140718)
  real(kind=kind_noahmp), public, parameter :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  real(kind=kind_noahmp), public, parameter :: RW     = 461.269   !gas constant for  water vapor (j/kg/k)
  real(kind=kind_noahmp), public, parameter :: DENH2O = 1000.0    !density of water (kg/m3)
  real(kind=kind_noahmp), public, parameter :: DENICE = 917.0     !density of ice (kg/m3)

end module ConstantDefineMod
