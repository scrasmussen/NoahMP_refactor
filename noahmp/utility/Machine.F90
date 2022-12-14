module Machine

!!! define machine-related constants and parameters
!!! To define real data type precision, use "-DOUBLE_PREC" in CPPFLAG in user_build_options file
!!! By default, Noah-MP uses single precision
  use netcdf, only : NF90_FILL_FLOAT, NF90_FILL_DOUBLE, NF90_FILL_INT
  implicit none
  save
  private

#ifdef DOUBLE_PREC
  integer, public, parameter :: kind_noahmp = 8 ! double precision
  real(kind=kind_noahmp), public, parameter :: undefined_real = NF90_FILL_DOUBLE
  ! for variable initializatin
#else
  integer, public, parameter :: kind_noahmp = 4 ! single precision
  real(kind=kind_noahmp), public, parameter :: undefined_real = NF90_FILL_FLOAT
  ! for variable initializatin
#endif

  integer,                public, parameter :: undefined_int  = huge(1)    ! undefined integer number for variable initialization


end module Machine
