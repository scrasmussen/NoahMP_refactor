module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine BiochemVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp



    ! biochem parameter variables
    noahmp%biochem%param%PLTDAY      = huge(1  )
    noahmp%biochem%param%HSDAY       = huge(1  )

  end subroutine BiochemVarInitDefault

!=== initialize with input data or table values
  subroutine BiochemVarInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(inout) :: input

    associate(                                                  &
              CROPTYP     => noahmp%config%domain%CROPTYP       &
             )


    ! biochem parameter variables
    noahmp%biochem%param%PLTDAY      = input%PLTDAY_TABLE(CROPTYP)
    noahmp%biochem%param%HSDAY       = input%HSDAY_TABLE (CROPTYP)

    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
