module GroundThermalPropertyGlacierMod

!!! Compute snow and glacier ice thermal conductivity and heat capacity

  use Machine
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
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     main noahmp timestep (s)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,     thickness of snow/soil layers (m)
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,     actual number of snow layers (negative)
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! in,     snow depth [m]
              ThermConductSoilSnow              => noahmp%energy%state%ThermConductSoilSnow              ,& ! out,    thermal conductivity [w/m/k] for all soil & snow
              HeatCapacSoilSnow           => noahmp%energy%state%HeatCapacSoilSnow           ,& ! out,    heat capacity [j/m3/k] for all soil & snow
              PhaseChgFacSoilSnow            => noahmp%energy%state%PhaseChgFacSoilSnow            ,& ! out,    energy factor for soil & snow phase change
              HeatCapacVolSnow           => noahmp%energy%state%HeatCapacVolSnow           ,& ! out,    snow layer volumetric specific heat (j/m3/k)
              ThermConductSnow           => noahmp%energy%state%ThermConductSnow           ,& ! out,    snow layer thermal conductivity (w/m/k)
              CVGLAICE        => noahmp%energy%state%CVGLAICE        ,& ! out,    glacier ice layer volumetric specific heat (j/m3/k)
              TKGLAICE        => noahmp%energy%state%TKGLAICE         & ! out,    glacier ice layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    ! compute snow thermal conductivity and heat capacity
    call SnowThermalProperty(noahmp)
    do IZ = NumSnowLayerNeg+1, 0
       ThermConductSoilSnow   (IZ) = ThermConductSnow(IZ)
       HeatCapacSoilSnow(IZ) = HeatCapacVolSnow(IZ)
    enddo

    ! compute glacier ice thermal properties (using Noah glacial ice approximations)
    call GlacierIceThermalProperty(noahmp)
    do IZ = 1, NumSoilLayer
       ThermConductSoilSnow   (IZ) = TKGLAICE(IZ)
       HeatCapacSoilSnow(IZ) = CVGLAICE(IZ)
    enddo

    ! combine a temporary variable used for melting/freezing of snow and glacier ice
    do IZ = NumSnowLayerNeg+1, NumSoilLayer
       PhaseChgFacSoilSnow(IZ) = MainTimeStep / (HeatCapacSoilSnow(IZ) * ThicknessSnowSoilLayer(IZ))
    enddo

    ! snow/glacier ice interface
    if ( NumSnowLayerNeg == 0 ) then
       ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + 0.35*SnowDepth) / (SnowDepth + ThicknessSnowSoilLayer(1))
    else
       ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + ThermConductSoilSnow(0)*ThicknessSnowSoilLayer(0)) / (ThicknessSnowSoilLayer(0) + ThicknessSnowSoilLayer(1))
    endif

    end associate

  end subroutine GroundThermalPropertyGlacier

end module GroundThermalPropertyGlacierMod
