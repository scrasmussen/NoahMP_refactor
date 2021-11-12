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
    integer                   :: OPT_RUN     ! options for surface runoff
                                             ! 1 -> TOPMODEL with groundwater; 2 -> TOPMODEL with an equilibrium water table;
                                             ! 3 -> original surface and subsurface runoff (free drainage);
                                             ! 4 -> BATS surface and subsurface runoff (free drainage)
                                             ! 5 -> Miguez-Macho&Fan groundwater scheme
                                             ! 6 -> Variable Infiltration Capacity Model surface runoff scheme
                                             ! 7 -> Xiananjiang Infiltration and surface runoff scheme 
                                             ! 8 -> Dynamic VIC surface runoff scheme
    integer                   :: OPT_INF     ! options for frozen soil permeability
                                             ! 1 -> linear effects, more permeable; 2 -> nonlinear effects, less permeable
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
    real(kind=kind_noahmp)    :: DX       ! noahmp model grid spacing (m)

    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSOIL   ! depth of layer-bottom from soil surface
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZSNSO  ! thickness of snow/soil layers (m)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSNSO   ! depth of snow/soil layer-bottom (m)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZLAYER  ! soil layer thickness (m)

  end type domain_type

!=== define config type that includes namelist & domain subtypes
  type, public :: config_type

    type(namelist_type) :: nmlist ! not using "namelist" to avoid issue with Fortran intrinsic namelist function
    type(domain_type)   :: domain

  end type config_type

end module ConfigVarType
