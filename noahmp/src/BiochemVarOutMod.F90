module BiochemVarOutMod

!!! Transfer column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables 
!!! to 2D NoahmpIO for output
!!! Biochemistry variables should be first defined in BiochemVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He, & refactor team (July, 2022)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output=====
  subroutine BiochemVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    associate(                                         &
              I    => noahmp%config%domain%GridIndexI ,&
              J    => noahmp%config%domain%GridIndexJ  &
             )

    NoahmpIO%LFMASSXY(I,J) = noahmp%biochem%state%LeafMass
    NoahmpIO%RTMASSXY(I,J) = noahmp%biochem%state%RootMass
    NoahmpIO%STMASSXY(I,J) = noahmp%biochem%state%StemMass
    NoahmpIO%WOODXY  (I,J) = noahmp%biochem%state%WoodMass
    NoahmpIO%STBLCPXY(I,J) = noahmp%biochem%state%CarbonMassDeepSoil
    NoahmpIO%FASTCPXY(I,J) = noahmp%biochem%state%CarbonMassShallowSoil
    NoahmpIO%NEEXY   (I,J) = noahmp%biochem%flux%NetEcoExchange
    NoahmpIO%GPPXY   (I,J) = noahmp%biochem%flux%GrossPriProduction
    NoahmpIO%NPPXY   (I,J) = noahmp%biochem%flux%NetPriProductionTot
    NoahmpIO%PSNXY   (I,J) = noahmp%biochem%flux%PhotosynTotal

    end associate

  end subroutine BiochemVarOutTransfer

end module BiochemVarOutMod
