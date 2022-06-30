module GroundThermalPropertyMod

!!! Compute snow and soil thermal conductivity and heat capacity

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowThermalPropertyMod, only : SnowThermalProperty
  use SoilThermalPropertyMod, only : SoilThermalProperty

  implicit none

contains

  subroutine GroundThermalProperty(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: THERMOPROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: IZ         ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     main noahmp timestep (s)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,     actual number of snow layers (negative)
              URBAN_FLAG      => noahmp%config%domain%URBAN_FLAG     ,& ! in,     logical flag for urban grid
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,     snow depth [m]
              STC             => noahmp%energy%state%STC             ,& ! in,     snow and soil layer temperature [k]
              DF              => noahmp%energy%state%DF              ,& ! out,    thermal conductivity [w/m/k] for all soil & snow
              HCPCT           => noahmp%energy%state%HCPCT           ,& ! out,    heat capacity [j/m3/k] for all soil & snow
              FACT            => noahmp%energy%state%FACT            ,& ! out,    energy factor for soil & snow phase change
              CVSNO           => noahmp%energy%state%CVSNO           ,& ! out,    snow layer volumetric specific heat (j/m3/k)
              TKSNO           => noahmp%energy%state%TKSNO           ,& ! out,    snow layer thermal conductivity (w/m/k)
              CVSOIL          => noahmp%energy%state%CVSOIL          ,& ! out,    soil layer volumetric specific heat (j/m3/k)
              TKSOIL          => noahmp%energy%state%TKSOIL           & ! out,    soil layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    ! compute snow thermal conductivity and heat capacity
    call SnowThermalProperty(noahmp)
    do IZ = NumSnowLayerNeg+1, 0
       DF   (IZ) = TKSNO(IZ)
       HCPCT(IZ) = CVSNO(IZ)
    enddo

    ! compute soil thermal properties
    call SoilThermalProperty(noahmp)
    do IZ = 1, NumSoilLayer
       DF   (IZ) = TKSOIL(IZ)
       HCPCT(IZ) = CVSOIL(IZ)
    enddo
    if ( URBAN_FLAG .eqv. .true. ) then
       do IZ = 1, NumSoilLayer
          DF(IZ) = 3.24
       enddo
    endif

    ! heat flux reduction effect from the overlying green canopy, adapted from 
    ! section 2.1.2 of Peters-Lidard et al. (1997, JGR, VOL 102(D4)).
    ! not in use because of the separation of the canopy layer from the ground.
    ! but this may represent the effects of leaf litter (Niu comments)
    ! DF(1) = DF(1) * EXP (SBETA * SHDFAC)

    ! compute lake thermal properties (no consideration of turbulent mixing for this version)
    if ( IST == 2 ) then
       do IZ = 1, NumSoilLayer
          if ( STC(IZ) > ConstFreezePoint) then
             HCPCT(IZ) = ConstHeatCapacWater
             DF(IZ)    = ConstThermConductWater  !+ KEDDY * ConstHeatCapacWater 
          else
             HCPCT(IZ) = ConstHeatCapacIce
             DF(IZ)    = ConstThermConductIce
          endif
       enddo
    endif

    ! combine a temporary variable used for melting/freezing of snow and frozen soil
    do IZ = NumSnowLayerNeg+1, NumSoilLayer
       FACT(IZ) = MainTimeStep / (HCPCT(IZ) * DZSNSO(IZ))
    enddo

    ! snow/soil interface
    if ( NumSnowLayerNeg == 0 ) then
       DF(1) = (DF(1)*DZSNSO(1) + 0.35*SNOWH) / (SNOWH + DZSNSO(1))
    else
       DF(1) = (DF(1)*DZSNSO(1) + DF(0)*DZSNSO(0)) / (DZSNSO(0) + DZSNSO(1))
    endif

    end associate

  end subroutine GroundThermalProperty

end module GroundThermalPropertyMod
