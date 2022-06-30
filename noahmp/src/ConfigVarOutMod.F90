module ConfigVarOutMod

!!! To transfer 1D Noah-MP column Config variables to 2D NoahmpIO for output
!!! Configuration variables should be first defined in ConfigVarType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 27, 2022)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output=====

  subroutine ConfigVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type) , intent(inout) :: NoahmpIO
    type(noahmp_type),    intent(inout) :: noahmp

    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ      ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer     &
             )

    NoahmpIO%ISNOWXY(I,J) = noahmp%config%domain%NumSnowLayerNeg
    NoahmpIO%ZSNSOXY(I,-NumSnowLayerMax+1:NumSoilLayer,J) = noahmp%config%domain%ZSNSO(-NumSnowLayerMax+1:NumSoilLayer)

    end associate

  end subroutine ConfigVarOutTransfer

end module ConfigVarOutMod
