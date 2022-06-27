module Machine

!!! define machine-related constants and parameters
!!! To define real data type precision, use "-DOUBLE_PREC" in CPPFLAG in user_build_options file
!!! By default, Noah-MP uses single precision

  implicit none
  save

#ifdef DOUBLE_PREC
  integer, parameter :: kind_noahmp = 8 ! double precision
#else
  integer, parameter :: kind_noahmp = 4 ! single precision
#endif

  integer,                parameter :: undefined_int  = huge(1)    ! undefined integer number for variable initialization
  real(kind=kind_noahmp), parameter :: undefined_real = huge(1.0)  ! undefined real number for variable initializatin

end module Machine
