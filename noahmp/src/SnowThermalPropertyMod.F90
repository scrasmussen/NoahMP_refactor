module SnowThermalPropertyMod

!!! Compute snowpack thermal conductivity and volumetric specific heat

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowThermalProperty(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CSNOW
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer               :: IZ       ! loop index
    real(kind=kind_noahmp), allocatable, dimension(:) :: BDSNOI  ! bulk density of snow(kg/m3)

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSnowLayerNeg     => noahmp%config%domain%NumSnowLayerNeg     ,& ! in,  actual number of snow layers (negative)
              NumSnowLayerMax     => noahmp%config%domain%NumSnowLayerMax     ,& ! in,  maximum number of snow layers
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,     thickness of snow/soil layers (m)
              OptSnowThermConduct => noahmp%config%nmlist%OptSnowThermConduct ,& ! in,  options for snow thermal conductivity schemes
              SnowIce           => noahmp%water%state%SnowIce            ,& ! in,     snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater            ,& ! in,     snow layer liquid water [mm]
              SnowIceVol          => noahmp%water%state%SnowIceVol           ,& ! out,    partial volume of snow ice [m3/m3]
              SnowLiqWaterVol          => noahmp%water%state%SnowLiqWaterVol           ,& ! out,    partial volume of snow liquid water [m3/m3]
              SnowEffPorosity           => noahmp%water%state%SnowEffPorosity       ,& ! out,    snow effective porosity [m3/m3]
              CVSNO           => noahmp%energy%state%CVSNO           ,& ! out,    snow layer volumetric specific heat (j/m3/k)
              TKSNO           => noahmp%energy%state%TKSNO            & ! out,    snow layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( BDSNOI(-NumSnowLayerMax+1:0) )
    BDSNOI = 0.0

    !  effective porosity of snow
    do IZ = NumSnowLayerNeg+1, 0
       SnowIceVol(IZ) = min( 1.0, SnowIce(IZ)/(ThicknessSnowSoilLayer(IZ)*ConstDensityIce) )
       SnowEffPorosity(IZ)  = 1.0 - SnowIceVol(IZ)
       SnowLiqWaterVol(IZ) = min( SnowEffPorosity(IZ), SnowLiqWater(IZ)/(ThicknessSnowSoilLayer(IZ)*ConstDensityWater) )
    enddo

    ! thermal capacity of snow
    do IZ = NumSnowLayerNeg+1, 0
       BDSNOI(IZ) = (SnowIce(IZ) + SnowLiqWater(IZ)) / ThicknessSnowSoilLayer(IZ)
       CVSNO(IZ)  = ConstHeatCapacIce * SnowIceVol(IZ) + ConstHeatCapacWater * SnowLiqWaterVol(IZ)
      ! CVSNO(IZ) = 0.525e06  ! constant
    enddo

    ! thermal conductivity of snow
    do IZ = NumSnowLayerNeg+1, 0
       if (OptSnowThermConduct == 1) TKSNO(IZ) = 3.2217e-6 * BDSNOI(IZ)**2.0               ! Stieglitz(yen,1965)
       if (OptSnowThermConduct == 2) TKSNO(IZ) = 2e-2 + 2.5e-6 * BDSNOI(IZ) * BDSNOI(IZ)   ! Anderson, 1976
       if (OptSnowThermConduct == 3) TKSNO(IZ) = 0.35                                      ! constant
       if (OptSnowThermConduct == 4) TKSNO(IZ) = 2.576e-6 * BDSNOI(IZ)**2.0 + 0.074        ! Verseghy (1991)
       if (OptSnowThermConduct == 5) TKSNO(IZ) = 2.22 * (BDSNOI(IZ)/1000.0)**1.88          ! Douvill(Yen, 1981)
    enddo

    end associate

  end subroutine SnowThermalProperty

end module SnowThermalPropertyMod
