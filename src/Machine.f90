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

end module Machine
