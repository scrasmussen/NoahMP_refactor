module SnowpackCompactionGlacierMod

!!! Snowpack compaction process over glacier
!!! Update snow depth via compaction due to destructive metamorphism, overburden, & melt

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowpackCompactionGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMPACT_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: J       ! snow layer index
    real(kind=kind_noahmp) :: BURDEN  ! pressure of overlying snow [kg/m2]
    real(kind=kind_noahmp) :: DEXPF   ! EXPF=exp(-c4*(273.15-STC))
    real(kind=kind_noahmp) :: TD      ! STC - ConstFreezePoint [K]
    real(kind=kind_noahmp) :: VOID    ! void (1 - SnowIce - SnowLiqWater)
    real(kind=kind_noahmp) :: WX      ! water mass (ice + liquid) [kg/m2]
    real(kind=kind_noahmp) :: BI      ! partial density of ice [kg/m3]

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              STC             => noahmp%energy%state%STC             ,& ! in,     snow and soil layer temperature [k]
              SnowIce           => noahmp%water%state%SnowIce            ,& ! in,     snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater            ,& ! in,     snow layer liquid water [mm]
              IndexPhaseChange           => noahmp%water%state%IndexPhaseChange            ,& ! in,     phase change index [0-none;1-melt;2-refreeze]
              SnowIceFracPrev         => noahmp%water%state%SnowIceFracPrev     ,& ! in,     ice fraction in snow layers at previous timestep
              SnowCompactBurdenFac              => noahmp%water%param%SnowCompactBurdenFac   ,& ! in,     snow overburden compaction parameter [m3/kg]
              SnowCompactAgingFac1              => noahmp%water%param%SnowCompactAgingFac1   ,& ! in,     snow desctructive metamorphism compaction parameter1 [1/s]
              SnowCompactAgingFac2              => noahmp%water%param%SnowCompactAgingFac2   ,& ! in,     snow desctructive metamorphism compaction parameter2 [1/k]
              SnowCompactAgingFac3              => noahmp%water%param%SnowCompactAgingFac3   ,& ! in,     snow desctructive metamorphism compaction parameter3 
              SnowCompactAgingMax              => noahmp%water%param%SnowCompactAgingMax   ,& ! in,     upper Limit on destructive metamorphism compaction [kg/m3]
              SnowViscosityCoeff            => noahmp%water%param%SnowViscosityCoeff ,& ! in,     snow viscosity coefficient [kg-s/m2], Anderson1979: 0.52e6~1.38e6
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! inout,  actual number of snow layers (negative)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! inout,  thickness of snow/soil layers (m)
              CompactionSnowAging            => noahmp%water%flux%CompactionSnowAging              ,& ! out,    rate of settling of snowpack due to destructive metamorphism [1/s]
              CompactionSnowBurden            => noahmp%water%flux%CompactionSnowBurden              ,& ! out,    rate of compaction of snowpack due to overburden [1/s]
              CompactionSnowMelt            => noahmp%water%flux%CompactionSnowMelt              ,& ! out,    rate of compaction of snowpack due to melt [1/s]
              CompactionSnowTot          => noahmp%water%flux%CompactionSnowTot            ,& ! out,    rate of change in fractional-thickness due to compaction [fraction/s]
              SnowIceFrac            => noahmp%water%state%SnowIceFrac         & ! out,    fraction of ice in snow layers at current time step 
             )
! ----------------------------------------------------------------------

! initialization for out-only variables
    CompactionSnowAging(:)   = 0.0
    CompactionSnowBurden(:)   = 0.0
    CompactionSnowMelt(:)   = 0.0
    CompactionSnowTot(:) = 0.0
    SnowIceFrac(:)   = 0.0

! start snow compaction
    BURDEN = 0.0
    do J = NumSnowLayerNeg+1, 0
       WX      = SnowIce(J) + SnowLiqWater(J)
       SnowIceFrac(J) = SnowIce(J) / WX
       VOID    = 1.0 - ( SnowIce(J)/ConstDensityIce + SnowLiqWater(J)/ConstDensityWater ) / ThicknessSnowSoilLayer(J)

       ! Allow compaction only for non-saturated node and higher ice lens node.
       if ( (VOID > 0.001) .and. (SnowIce(J) > 0.1) ) then
          BI    = SnowIce(J) / ThicknessSnowSoilLayer(J)
          TD    = max( 0.0, ConstFreezePoint-STC(J) )

          ! Settling/compaction as a result of destructive metamorphism
          DEXPF   = exp( -SnowCompactAgingFac2 * TD )
          CompactionSnowAging(J) = -SnowCompactAgingFac1 * DEXPF
          if ( BI > SnowCompactAgingMax ) &
             CompactionSnowAging(J) = CompactionSnowAging(J) * exp( -46.0e-3 * (BI-SnowCompactAgingMax) )
          if ( SnowLiqWater(J) > (0.01*ThicknessSnowSoilLayer(J)) ) &
             CompactionSnowAging(J) = CompactionSnowAging(J) * SnowCompactAgingFac3   ! Liquid water term

          ! Compaction due to overburden
          CompactionSnowBurden(J) = -(BURDEN + 0.5*WX) * &
                                    exp(-0.08*TD - SnowCompactBurdenFac*BI) / SnowViscosityCoeff ! 0.5*WX -> self-burden

          ! Compaction occurring during melt
          if ( IndexPhaseChange(J) == 1 ) then
             CompactionSnowMelt(J) = max( 0.0, (SnowIceFracPrev(J)-SnowIceFrac(J)) / max(1.0e-6,SnowIceFracPrev(J)) )
             CompactionSnowMelt(J) = -CompactionSnowMelt(J) / MainTimeStep   ! sometimes too large
          else
             CompactionSnowMelt(J) = 0.0
          endif

          ! Time rate of fractional change in DZ (units of s-1)
          CompactionSnowTot(J) = ( CompactionSnowAging(J) + CompactionSnowBurden(J) + CompactionSnowMelt(J) ) * MainTimeStep
          CompactionSnowTot(J) = max( -0.5, CompactionSnowTot(J) )

          ! The change in DZ due to compaction
          ThicknessSnowSoilLayer(J) = ThicknessSnowSoilLayer(J) * ( 1.0 + CompactionSnowTot(J) )
          ThicknessSnowSoilLayer(J) = max( ThicknessSnowSoilLayer(J), SnowIce(J)/ConstDensityIce + &
                                                                      SnowLiqWater(J)/ConstDensityWater )

       endif

       ! Pressure of overlying snow
       BURDEN = BURDEN + WX

    enddo

    end associate

  end subroutine SnowpackCompactionGlacier

end module SnowpackCompactionGlacierMod
