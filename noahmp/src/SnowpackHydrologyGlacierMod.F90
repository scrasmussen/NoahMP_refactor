module SnowpackHydrologyGlacierMod

!!! Snowpack hydrology processes (sublimation/frost, evaporation/dew, meltwater)
!!! Update snowpack ice and liquid water content

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerCombineGlacierMod, only : SnowLayerCombineGlacier

  implicit none

contains

  subroutine SnowpackHydrologyGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWH2O_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: J         ! do loop/array indices
    real(kind=kind_noahmp) :: InflowSnowLayer       ! water flow into each snow layer (mm/s)
    real(kind=kind_noahmp) :: QOUT      ! water flow out of each snow layer (mm/s)
    real(kind=kind_noahmp) :: WGDIF     ! ice mass after minus sublimation
    real(kind=kind_noahmp) :: PROPOR    ! ratio of SWE after frost & sublimation to original SWE
    real(kind=kind_noahmp) :: TEMP      ! temporary SWE
    real(kind=kind_noahmp), allocatable, dimension(:) :: VOL_LIQ   ! partial volume of liquid water in layer
    real(kind=kind_noahmp), allocatable, dimension(:) :: VOL_ICE   ! partial volume of ice lens in layer

! --------------------------------------------------------------------
    associate(                                                        &
              OptGlacierTreatment => noahmp%config%nmlist%OptGlacierTreatment ,& ! in,    option for glacier treatment
              NumSnowLayerMax     => noahmp%config%domain%NumSnowLayerMax     ,& ! in,    maximum number of snow layers
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              FSH             => noahmp%energy%flux%FSH              ,& ! in,     total sensible heat (w/m2) [+ to atm]
              FrostSnowSfcIce          => noahmp%water%flux%FrostSnowSfcIce            ,& ! in,     snow surface frost rate [mm/s]
              SublimSnowSfcIce          => noahmp%water%flux%SublimSnowSfcIce            ,& ! in,     snow surface sublimation rate [mm/s]
              RainfallGround           => noahmp%water%flux%RainfallGround             ,& ! in,     ground surface rain rate [mm/s]
              SNLIQMAXFRAC    => noahmp%water%param%SNLIQMAXFRAC     ,& ! in,     maximum liquid water fraction in snow
              SSI             => noahmp%water%param%SSI              ,& ! in,     liquid water holding capacity for snowpack (m3/m3)
              SNOW_RET_FAC    => noahmp%water%param%SNOW_RET_FAC     ,& ! in,     snowpack water release timescale factor (1/s)
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! inout,  actual number of snow layers (negative)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! inout,  thickness of snow/soil layers (m)
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout,  snow depth [m]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout,  snow water equivalent [mm]
              SnowIce           => noahmp%water%state%SnowIce            ,& ! inout,  snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater           ,& ! inout,  snow layer liquid water [mm]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil liquid moisture (m3/m3)
              SoilIce            => noahmp%water%state%SoilIce             ,& ! inout,  soil ice moisture (m3/m3)
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              SnowEffPorosity           => noahmp%water%state%SnowEffPorosity       ,& ! out,    snow effective porosity (m3/m3)
              SnowBotOutflow          => noahmp%water%flux%SnowBotOutflow             & ! out,   total water (snowmelt + rain through pack) out of snowpack bottom [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( VOL_LIQ(-NumSnowLayerMax+1:0) )
    allocate( VOL_ICE(-NumSnowLayerMax+1:0) )
    VOL_LIQ(:) = 0.0
    VOL_ICE(:) = 0.0
    SnowEffPorosity  (:) = 0.0
    SnowBotOutflow     = 0.0
    InflowSnowLayer        = 0.0
    QOUT       = 0.0

    ! for the case when SnowWaterEquiv becomes '0' after 'COMBINE'
    if ( SnowWaterEquiv == 0.0 ) then
       if ( OptGlacierTreatment == 1 ) then
          SoilIce(1) =  SoilIce(1) + (FrostSnowSfcIce - SublimSnowSfcIce) * MainTimeStep / (ThicknessSnowSoilLayer(1)*1000.0)  ! Barlage: SoilLiqWater->SoilIce v3.6
       elseif ( OptGlacierTreatment == 2 ) then
          FSH    = FSH - (FrostSnowSfcIce - SublimSnowSfcIce) * ConstLatHeatSublim
          FrostSnowSfcIce = 0.0
          SublimSnowSfcIce = 0.0
       endif
    endif

    ! for shallow snow without a layer
    ! snow surface sublimation may be larger than existing snow mass. To conserve water,
    ! excessive sublimation is used to reduce soil water. Smaller time steps would tend to aviod this problem.
    if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) ) then
       if ( OptGlacierTreatment == 1 ) then
          TEMP   = SnowWaterEquiv
          SnowWaterEquiv  = SnowWaterEquiv - SublimSnowSfcIce*MainTimeStep + FrostSnowSfcIce*MainTimeStep
          PROPOR = SnowWaterEquiv / TEMP
          SnowDepth  = max( 0.0, PROPOR*SnowDepth )
          SnowDepth  = min( max(SnowDepth, SnowWaterEquiv/500.0), SnowWaterEquiv/50.0 )  ! limit adjustment to a reasonable density
       elseif ( OptGlacierTreatment == 2 ) then
          FSH = FSH - (FrostSnowSfcIce - SublimSnowSfcIce) * ConstLatHeatSublim
          FrostSnowSfcIce = 0.0
          SublimSnowSfcIce = 0.0
       endif
       if ( SnowWaterEquiv < 0.0 ) then
          SoilIce(1) = SoilIce(1) + SnowWaterEquiv / (ThicknessSnowSoilLayer(1)*1000.0)
          SnowWaterEquiv   = 0.0
          SnowDepth   = 0.0
       endif
       if ( SoilIce(1) < 0.0 ) then
          SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
          SoilIce(1) = 0.0
       endif
    endif

    if ( (SnowDepth <= 1.0e-8) .or. (SnowWaterEquiv <= 1.0e-6) ) then
       SnowDepth = 0.0
       SnowWaterEquiv = 0.0
    endif

    ! for multi-layer (>=1) snow
    if ( NumSnowLayerNeg < 0 ) then
      WGDIF          = SnowIce(NumSnowLayerNeg+1) - SublimSnowSfcIce*MainTimeStep + FrostSnowSfcIce*MainTimeStep
      SnowIce(NumSnowLayerNeg+1) = WGDIF
      if ( (WGDIF < 1.0e-6) .and. (NumSnowLayerNeg < 0) ) call SnowLayerCombineGlacier(noahmp)
      if ( NumSnowLayerNeg < 0 ) then
         SnowLiqWater(NumSnowLayerNeg+1) = SnowLiqWater(NumSnowLayerNeg+1) + RainfallGround * MainTimeStep
         SnowLiqWater(NumSnowLayerNeg+1) = max( 0.0, SnowLiqWater(NumSnowLayerNeg+1) )
      endif
    endif

    ! Porosity and partial volume
    do J = NumSnowLayerNeg+1, 0
       VOL_ICE(J) = min( 1.0, SnowIce(J)/(ThicknessSnowSoilLayer(J)*ConstDensityIce) )
       SnowEffPorosity(J)   = 1.0 - VOL_ICE(J)
    enddo

    ! compute inter-layer snow water flow
    do J = NumSnowLayerNeg+1, 0
       SnowLiqWater(J)   = SnowLiqWater(J) + InflowSnowLayer
       VOL_LIQ(J) = SnowLiqWater(J) / (ThicknessSnowSoilLayer(J)*ConstDensityWater)
       QOUT       = max( 0.0, (VOL_LIQ(J) - SSI*SnowEffPorosity(J)) * ThicknessSnowSoilLayer(J) )
       if ( J == 0 ) then
          QOUT = max( (VOL_LIQ(J) - SnowEffPorosity(J)) * ThicknessSnowSoilLayer(J), SNOW_RET_FAC * MainTimeStep * QOUT )
       endif
       QOUT     = QOUT * ConstDensityWater
       SnowLiqWater(J) = SnowLiqWater(J) - QOUT
       if ( ( SnowLiqWater(J) / (SnowIce(J)+SnowLiqWater(J)) ) > SNLIQMAXFRAC ) then
          QOUT     = QOUT + ( SnowLiqWater(J) - SNLIQMAXFRAC/(1.0-SNLIQMAXFRAC) * SnowIce(J) )
          SnowLiqWater(J) = SNLIQMAXFRAC / (1.0 - SNLIQMAXFRAC) * SnowIce(J)
       endif
       InflowSnowLayer = QOUT
    enddo

    ! update snow depth
    do J = NumSnowLayerNeg+1, 0
       ThicknessSnowSoilLayer(J) = max( ThicknessSnowSoilLayer(J), SnowLiqWater(J)/ConstDensityWater + SnowIce(J)/ConstDensityIce )
    enddo

    ! Liquid water from snow bottom to soil (mm/s)
    SnowBotOutflow = QOUT / MainTimeStep

    end associate

  end subroutine SnowpackHydrologyGlacier

end module SnowpackHydrologyGlacierMod
