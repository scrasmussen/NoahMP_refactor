module InputType
!!! Define Noah-MP Input variables (2D forcing, namelist, table, static)
!!! Input variable initialization is done in InputInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

  type, public :: input_type

    !--------------Domain Config------------------------------------------------------------
    character(len=512) :: flnm_template
    integer            :: XSTART
    integer            :: XEND
    integer            :: YSTART
    integer            :: YEND
    integer            :: KDS
    integer            :: KDE
    integer            :: KTS
    integer            :: KTE
    integer            :: NX
    integer            :: NY
    integer            :: NZ
    integer            :: IOPT_RUN    
    integer            :: IOPT_SNF  
 
    real               :: timestep

    !--------------Domain Specific Variables---------------------------------------------------------------
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: COSZIN                   

    !--------------Met- Forcing input variables------------------------------------------------------------
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  :: U_PHY     ! 3D U wind component [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  :: V_PHY     ! 3D V wind component [m/s]   
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  :: T3D       ! 3D atmospheric temperature valid at mid-levels [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  :: QV3D      ! 3D water vapor mixing ratio [kg/kg_dry]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  :: P8W3D     ! 3D pressure, valid at interface [Pa]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SWDOWN    ! solar down at surface [W m-2] 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SWDDIF    ! solar down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SWDDIR    ! solar down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: GLW       ! longwave down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: PRECIP_IN ! total input precipitation [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SR        ! frozen precipitation ratio [-]

    !optional detailed precipitation partitioning inputs
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MP_RAINC  ! convective precipitation entering land model [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MP_RAINNC ! large-scale precipitation entering land model [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MP_SHCV   ! shallow conv precip entering land model [mm] 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MP_SNOW   ! snow precipitation entering land model [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MP_GRAUP  ! graupel precipitation entering land model [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MP_HAIL   ! hail precipitation entering land model [mm]

    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: QSFC
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: TMN
   
    !--------------enegy variables--------------------------------------------------------------------------
    real(kind=kind_noahmp), allocatable, dimension(:,:)   :: SAV2D
    

  end type input_type

end module InputType
