module ConfigVarType

!!! Define column (1-D) Noah-MP configuration variables
!!! Configuration variable initialization is done in ConfigInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "namelist" sub-type of config_type (config%nmlist%variable)
  type :: namelist_type

    ! define specific namelist variables
    integer                   :: OPT_INFDV   ! options for infiltration in dynamic VIC runoff scheme
                                             ! 1 -> Philip scheme (default); 2 -> Green-Ampt scheme; 3 -> Smith-Parlange scheme    

  end type namelist_type

!=== define "domain" sub-type of config_type (config%domain%variable)
  type :: domain_type

    ! define specific domain variables
    integer                   :: ILOC     ! model grid index
    integer                   :: JLOC     ! model grid index
    integer                   :: VEGTYP   ! vegetation type
    integer                   :: NSOIL    ! number of soil layers
    integer                   :: NSNOW    ! maximum number of snow layers
    integer                   :: ISNOW    ! actual number of snow layers
    real(kind=kind_noahmp)    :: DT       ! noahmp timestep (s)

    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSOIL   ! depth of layer-bottom from soil surface
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZSNSO  ! thickness of snow/soil layers (m)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSNSO   ! depth of snow/soil layer-bottom (m)

  end type domain_type

!=== define config type that includes namelist & domain subtypes
  type, public :: config_type

    type(namelist_type) :: nmlist ! not using "namelist" to avoid issue with Fortran intrinsic namelist function
    type(domain_type)   :: domain

  end type config_type

end module ConfigVarType
