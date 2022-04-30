module ConfigVarOutMod

!!! To transfer 1D Noah-MP column Config variables to 2D NoahmpIO for output
!!! Configuration variables should be first defined in ConfigVarType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 27, 2022)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
  use NoahmpVarType
  use Machine, only : kind_noahmp

  implicit none

contains

!=== Transfer model states to output=====

  subroutine ConfigVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type) , intent(inout) :: NoahmpIO
    type(noahmp_type),    intent(inout) :: noahmp

    associate(                                      &
              I     => NoahmpIO%I                  ,&
              J     => NoahmpIO%J                  ,&
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    NoahmpIO%ISNOWXY  (I,J)                = noahmp%config%domain%ISNOW
    NoahmpIO%ZSNSOXY  (I,-NSNOW+1:NSOIL,J) = noahmp%config%domain%ZSNSO (-NSNOW+1:NSOIL)

    end associate

  end subroutine ConfigVarOutTransfer

end module ConfigVarOutMod
