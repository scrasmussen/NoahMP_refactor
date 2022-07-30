module ResistanceGroundEvaporationMod

!!! Compute soil surface resistance to ground evaporation/sublimation
!!! It represents the resistance imposed by the molecular diffusion in soil 
!!! surface (as opposed to aerodynamic resistance computed elsewhere in the model)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceGroundEvaporation(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: SoilEvapFac        ! soil water evaporation factor (0- 1)
    real(kind=kind_noahmp)           :: L_RSURF      ! Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
    real(kind=kind_noahmp)           :: D_RSURF      ! Reduced vapor diffusivity in soil for computing RSURF (SZ09)
    real(kind=kind_noahmp)           :: SoilMatPotentialSfc          ! surface layer soil matric potential [m]

! --------------------------------------------------------------------
    associate(                                                        &
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,    depth [m] of layer-bottom from soil surface
              FlagUrban      => noahmp%config%domain%FlagUrban     ,& ! in,    logical flag for urban grid
              OptGroundResistanceEvap => noahmp%config%nmlist%OptGroundResistanceEvap,& ! in,    options for ground resistance to evaporation/sublimation
              ResistanceSoilExp       => noahmp%energy%param%ResistanceSoilExp       ,& ! in,    exponent in the shape parameter for soil resistance
              ResistanceSnowSfc      => noahmp%energy%param%ResistanceSnowSfc      ,& ! in,    surface resistance for snow(s/m)
              SoilMoistureSat          => noahmp%water%param%SoilMoistureSat           ,& ! in,    saturated value of soil moisture [m3/m3]
              SoilMoistureWilt          => noahmp%water%param%SoilMoistureWilt           ,& ! in,    wilting point soil moisture [m3/m3]
              SoilExpCoeffB            => noahmp%water%param%SoilExpCoeffB             ,& ! in,    soil B parameter
              SoilMatPotentialSat          => noahmp%water%param%SoilMatPotentialSat           ,& ! in,    saturated soil matric potential (m)
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,    soil water content [m3/m3]
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,    snow cover fraction [-]
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! in,    snow depth [m]
              TemperatureGrd              => noahmp%energy%state%TemperatureGrd              ,& ! in,    ground temperature (K)
              RSURF           => noahmp%energy%state%RSURF           ,& ! out,   ground surface resistance (s/m)
              RHSUR           => noahmp%energy%state%RHSUR            & ! out,   raltive humidity in surface soil/snow air space (-)
             )
! ----------------------------------------------------------------------

    ! initialization
    SoilEvapFac = max( 0.0, SoilLiqWater(1)/SoilMoistureSat(1) )
    if ( SurfaceType == 2 ) then  ! lake point
       RSURF = 1.0        ! avoid being divided by 0
       RHSUR = 1.0
    else    ! soil point

       if ( (OptGroundResistanceEvap == 1) .or. (OptGroundResistanceEvap == 4) ) then   ! Sakaguchi and Zeng, 2009
          ! taking the "residual water content" to be the wilting point, 
          ! and correcting the exponent on the D term (typo in SZ09 ?)
          L_RSURF = (-DepthSoilLayer(1)) * &
                    (exp((1.0-min(1.0,SoilLiqWater(1)/SoilMoistureSat(1))) ** ResistanceSoilExp)-1.0) / (2.71828-1.0)
          D_RSURF = 2.2e-5 * SoilMoistureSat(1) * SoilMoistureSat(1) * &
                    (1.0 - SoilMoistureWilt(1)/SoilMoistureSat(1)) ** (2.0 + 3.0/SoilExpCoeffB(1))
          RSURF = L_RSURF / D_RSURF
       elseif ( OptGroundResistanceEvap == 2 ) then  ! Sellers (1992) original
          RSURF = SnowCoverFrac * 1.0 + (1.0 - SnowCoverFrac) * exp(8.25 - 4.225*SoilEvapFac)
       elseif ( OptGroundResistanceEvap == 3 ) then  ! Sellers (1992) adjusted to decrease RSURF for wet soil
          RSURF = SnowCoverFrac * 1.0 + (1.0 - SnowCoverFrac) * exp(8.25 - 6.0*SoilEvapFac) 
       endif
       if ( OptGroundResistanceEvap == 4 ) then ! SnowCoverFrac weighted; snow RSURF set in MPTABLE v3.8
          RSURF = 1.0 / ( SnowCoverFrac * (1.0/ResistanceSnowSfc) + (1.0 - SnowCoverFrac) * (1.0/max(RSURF,0.001)) )
       endif
       if ( (SoilLiqWater(1) < 0.01) .and. (SnowDepth == 0.0) ) RSURF = 1.0e6

       SoilMatPotentialSfc = -SoilMatPotentialSat(1) * (max(0.01,SoilLiqWater(1)) / SoilMoistureSat(1)) ** (-SoilExpCoeffB(1))
       RHSUR = SnowCoverFrac + (1.0 - SnowCoverFrac) * exp(SoilMatPotentialSfc * ConstGravityAcc / (ConstGasWaterVapor * TemperatureGrd))
    endif

    ! urban
    if ( (FlagUrban .eqv. .true.) .and. (SnowDepth == 0.0) ) then
       RSURF = 1.0e6
    endif

    end associate

  end subroutine ResistanceGroundEvaporation

end module ResistanceGroundEvaporationMod
