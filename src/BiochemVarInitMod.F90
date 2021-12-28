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


    ! biochem state variables
    noahmp%biochem%state%IGS            = huge(1.0)
    noahmp%biochem%state%FOLN           = huge(1.0)

    ! biochem flux variables
    noahmp%biochem%flux%PSNSUN          = huge(1.0)
    noahmp%biochem%flux%PSNSHA          = huge(1.0)

    ! biochem parameter variables
    noahmp%biochem%param%PLTDAY         = huge(1  )
    noahmp%biochem%param%HSDAY          = huge(1  )
    noahmp%biochem%param%FOLNMX         = huge(1.0)
    noahmp%biochem%param%QE25           = huge(1.0)
    noahmp%biochem%param%VCMX25         = huge(1.0)
    noahmp%biochem%param%AVCMX          = huge(1.0)
    noahmp%biochem%param%C3PSN          = huge(1.0)
    noahmp%biochem%param%MP             = huge(1.0)

  end subroutine BiochemVarInitDefault

!=== initialize with input data or table values
  subroutine BiochemVarInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(inout) :: input

    associate(                                                  &
              ILOC        => noahmp%config%domain%ILOC         ,&
              JLOC        => noahmp%config%domain%JLOC         ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              CROPTYP     => noahmp%config%domain%CROPTYP      ,&
              SOILCOLOR   => noahmp%config%domain%SOILCOLOR    ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL        ,&
              NBAND       => noahmp%config%domain%NBAND         &
             )

    ! biochem state variables
    noahmp%biochem%state%FOLN           = 1.0  ! default value (%) for now

    ! biochem parameter variables
    noahmp%biochem%param%FOLNMX         = input%FOLNMX_TABLE(VEGTYP)
    noahmp%biochem%param%QE25           = input%QE25_TABLE(VEGTYP)
    noahmp%biochem%param%VCMX25         = input%VCMX25_TABLE(VEGTYP)
    noahmp%biochem%param%AVCMX          = input%AVCMX_TABLE(VEGTYP)
    noahmp%biochem%param%C3PSN          = input%C3PSN_TABLE(VEGTYP)
    noahmp%biochem%param%MP             = input%MP_TABLE(VEGTYP)

    if ( CROPTYP > 0 ) then
       noahmp%biochem%param%PLTDAY      = input%PLTDAY_TABLE(CROPTYP)
       noahmp%biochem%param%HSDAY       = input%HSDAY_TABLE (CROPTYP)
       noahmp%biochem%param%FOLNMX      = input%FOLNMXI_TABLE(CROPTYP)
       noahmp%biochem%param%QE25        = input%QE25I_TABLE(CROPTYP)
       noahmp%biochem%param%VCMX25      = input%VCMX25I_TABLE(CROPTYP)
       noahmp%biochem%param%AVCMX       = input%AVCMXI_TABLE(CROPTYP)
       noahmp%biochem%param%C3PSN       = input%C3PSNI_TABLE(CROPTYP)
       noahmp%biochem%param%MP          = input%MPI_TABLE(CROPTYP)

    endif


    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
