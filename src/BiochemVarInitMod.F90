module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemType.f90

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine BiochemVarInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%biochem%state%IGS = huge(1.0)
    noahmp%biochem%state%PGS = huge(1  )

    noahmp%biochem%param%TMIN = huge(1.0)

  end subroutine BiochemVarInitDefault

!=== initialize with input data or table values
  subroutine BiochemVarInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    associate(VEGTYP => noahmp%config%domain%VEGTYP)

    noahmp%biochem%state%PGS  = input%pgs

    noahmp%biochem%param%TMIN = input%TMIN_TABLE(VEGTYP)  

    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
