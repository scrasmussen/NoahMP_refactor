module WaterVarInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine WaterVarInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    associate(                                      &
      NSNOW => noahmp%config%domain%NSNOW  ,&
      NSOIL => noahmp%config%domain%NSOIL   &
    )

    allocate( noahmp%water%state%SMC      (1:NSOIL) )
    allocate( noahmp%water%param%SMCMAX   (1:NSOIL) )

    noahmp%water%state%SNOWH      = huge(1.0)
    noahmp%water%state%BTRAN      = huge(1.0) 
    noahmp%water%state%WROOT      = huge(1.0) 
    noahmp%water%state%WSTRES     = huge(1.0)
    noahmp%water%state%SMC(:)     = huge(1.0)
    noahmp%water%param%SMCMAX(:)  = huge(1.0)
    noahmp%water%param%NROOT      = huge(1  )

    end associate

  end subroutine WaterVarInitDefault

!=== initialize with input data or table values
  subroutine WaterVarInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    ! local loop index
    integer                          :: ISOIL

    associate(                                                  &
      ILOC        => noahmp%config%domain%ILOC         ,&
      JLOC        => noahmp%config%domain%JLOC         ,&
      NSNOW       => noahmp%config%domain%NSNOW        ,&
      NSOIL       => noahmp%config%domain%NSOIL        ,&
      VEGTYP      => noahmp%config%domain%VEGTYP       ,&
      SOILTYP     => noahmp%config%domain%SOILTYP      ,&
      URBAN_FLAG  => noahmp%config%domain%URBAN_FLAG    &
    )

    noahmp%water%state%SNOWH  = input%SNOWH
    noahmp%water%state%BTRAN  = input%btran

    noahmp%water%param%NROOT  = input%NROOT_TABLE(VEGTYP)
    
    do ISOIL = 1, size(SOILTYP)
      noahmp%water%param%SMCMAX(ISOIL)   = input%SMCMAX_TABLE(SOILTYP(ISOIL))
    enddo

    if ( URBAN_FLAG .eqv. .true. ) then
      noahmp%water%param%SMCMAX(:) = 0.45
    endif

    end associate

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
