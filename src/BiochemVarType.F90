module BiochemVarType

!!! Define column (1-D) Noah-MP Biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variable initialization is done in BiochemInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of biochem_type (biochem%flux%variable)
  type :: flux_type

    ! define specific biochem flux variables
    real(kind=kind_noahmp) :: PSNSUN           ! sunlit leaf photosynthesis (umol co2 /m2/ s)
    real(kind=kind_noahmp) :: PSNSHA           ! shaded leaf photosynthesis (umol co2 /m2 /s)
    real(kind=kind_noahmp) :: PSN              ! total leaf photosynthesis (umol co2 /m2 /s)

  end type flux_type

!=== define "state" sub-type of biochem_type (biochem%state%variable)
  type :: state_type

    ! define specific biochem state variables
    real(kind=kind_noahmp) :: IGS              ! growing season index (0=off, 1=on)    
    real(kind=kind_noahmp) :: FOLN             ! foliage nitrogen concentration (%)

  end type state_type

!=== define "parameter" sub-type of biochem_type (biochem%param%variable)
  type :: parameter_type

    ! define specific biochem parameter variables
    integer                :: PLTDAY           ! Planting date
    integer                :: HSDAY            ! Harvest date
    real(kind=kind_noahmp) :: FOLNMX           ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp) :: QE25             ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp) :: VCMX25           ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp) :: AVCMX            ! q10 for vcmx25
    real(kind=kind_noahmp) :: C3PSN            ! photosynthetic pathway: 0. = c4, 1. = c3
    real(kind=kind_noahmp) :: MP               ! slope of conductance-to-photosynthesis relationship

  end type parameter_type

!=== define "diagnose" sub-type of biochem_type (biochem%diag%variable)
  type :: diagnose_type

    ! define specific biochem diagnose variables

  end type diagnose_type

!=== define biochem type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: biochem_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type biochem_type

end module BiochemVarType
