module ConfigVarType

!!! Define column (1-D) Noah-MP configuration variables
!!! Configuration variable initialization is done in ConfigInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "namelist" sub-type of config_type (config%nmlist%variable)
  type :: namelist_type

    ! define specific namelist variables

    integer                   :: DVEG  

  end type namelist_type

!=== define "domain" sub-type of config_type (config%domain%variable)
  type :: domain_type

    ! define specific domain variables

    logical                   :: URBAN_FLAG  ! flag for urban grid
    integer                   :: ILOC        ! model grid index
    integer                   :: JLOC        ! model grid index
    integer                   :: NSOIL       ! number of soil layers
    integer                   :: NSNOW       ! maximum number of snow layers
    integer                   :: ISWATER     ! flag to identify water
    integer                   :: ISBARREN    ! flag to identify barren land
    integer                   :: ISICE       ! flag to identify ice
    integer                   :: ISCROP      ! flag to identify crop
    integer                   :: EBLFOREST   ! flag to identify EBL Forest
    integer                   :: NSTAGE      ! number of growth stages
    integer                   :: VEGTYP      ! vegetation type
    integer                   :: SOILTYP     ! soil type   
    integer                   :: CROPTYP     ! crop type
    integer                   :: YEARLEN     ! Number of days in the particular year
    integer                   :: IST         ! surface type 1->soil; 2->lake

    real(kind=kind_noahmp)    :: JULIAN      ! julian day of the year
    real(kind=kind_noahmp)    :: LAT         ! latitude (radians)
    real(kind=kind_noahmp)    :: DT          ! noahmp timestep (s)

    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSOIL   ! depth of layer-bottom from soil surface
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZSNSO  ! thickness of snow/soil layers (m)

  end type domain_type

!=== define config type that includes namelist & domain subtypes
  type, public :: config_type

    type(namelist_type) :: nmlist ! not using "namelist" to avoid issue with Fortran intrinsic namelist function
    type(domain_type)   :: domain

  end type config_type

end module ConfigVarType
