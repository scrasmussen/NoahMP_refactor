module GroundThermalPropertyGlacierMod

!!! Compute snow and glacier ice thermal conductivity and heat capacity

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SnowThermalPropertyMod,       only : SnowThermalProperty
  use GlacierIceThermalPropertyMod, only : GlacierIceThermalProperty

  implicit none

contains

  subroutine GroundThermalPropertyGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: THERMOPROP_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ         ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     maximum number of soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,     maximum number of snow layers
              IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake
              DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,     actual number of snow layers
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,     snow depth [m]
              STC             => noahmp%energy%state%STC             ,& ! in,     snow and soil layer temperature [k]
              DF              => noahmp%energy%state%DF              ,& ! out,    thermal conductivity [w/m/k] for all soil & snow
              HCPCT           => noahmp%energy%state%HCPCT           ,& ! out,    heat capacity [j/m3/k] for all soil & snow
              FACT            => noahmp%energy%state%FACT            ,& ! out,    energy factor for soil & snow phase change
              CVSNO           => noahmp%energy%state%CVSNO           ,& ! out,    snow layer volumetric specific heat (j/m3/k)
              TKSNO           => noahmp%energy%state%TKSNO           ,& ! out,    snow layer thermal conductivity (w/m/k)
              CVGLAICE        => noahmp%energy%state%CVGLAICE        ,& ! out,    glacier ice layer volumetric specific heat (j/m3/k)
              TKGLAICE        => noahmp%energy%state%TKGLAICE         & ! out,    glacier ice layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    ! compute snow thermal conductivity and heat capacity
    call SnowThermalProperty(noahmp)
    do IZ = ISNOW+1, 0
       DF   (IZ) = TKSNO(IZ)
       HCPCT(IZ) = CVSNO(IZ)
    enddo

    ! compute glacier ice thermal properties (using Noah glacial ice approximations)
    call GlacierIceThermalProperty(noahmp)
    do IZ = 1, NSOIL
       DF   (IZ) = TKGLAICE(IZ)
       HCPCT(IZ) = CVGLAICE(IZ)
    enddo

    ! combine a temporary variable used for melting/freezing of snow and glacier ice
    do IZ = ISNOW+1, NSOIL
       FACT(IZ) = DT / (HCPCT(IZ) * DZSNSO(IZ))
    enddo

    ! snow/glacier ice interface
    if ( ISNOW == 0 ) then
       DF(1) = (DF(1)*DZSNSO(1) + 0.35*SNOWH) / (SNOWH + DZSNSO(1))
    else
       DF(1) = (DF(1)*DZSNSO(1) + DF(0)*DZSNSO(0)) / (DZSNSO(0) + DZSNSO(1))
    endif

    end associate

  end subroutine GroundThermalPropertyGlacier

end module GroundThermalPropertyGlacierMod
