module SoilWaterTranspirationMod

!!! compute soil water transpiration factor that will be used for 
!!! stomata resistance and evapotranspiration calculations

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilWaterTranspiration(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    integer                          :: IZ       ! loop index
    real(kind=kind_noahmp)           :: GX       ! temporary variable
    real(kind=kind_noahmp)           :: MPE      ! minimum threshold to prevent divided by zero

! --------------------------------------------------------------------
    associate(                                                        &
              SurfaceType     => noahmp%config%domain%SurfaceType    ,& ! in,    surface type 1-soil; 2-lake
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers (m)
              DepthSoilLayer  => noahmp%config%domain%DepthSoilLayer ,& ! in,    depth [m] of layer-bottom from soil surface
              OptSoilWaterTranspiration => noahmp%config%nmlist%OptSoilWaterTranspiration,& ! in,    options for soil moisture factor for stomatal resistance & ET
              NROOT           => noahmp%water%param%NROOT            ,& ! in,    number of soil layers with root present
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,    wilting point soil moisture [m3/m3]
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,    reference soil moisture (field capacity) (m3/m3)
              PSIWLT          => noahmp%water%param%PSIWLT           ,& ! in,    soil metric potential for wilting point (m)
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,    saturated soil matric potential (m)
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,    saturated value of soil moisture [m3/m3]
              BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter              
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,    soil water content [m3/m3]
              SoilTranspFac          => noahmp%water%state%SoilTranspFac           ,& ! out,   soil water transpiration factor (0 to 1)
              SoilTranspFacAcc           => noahmp%water%state%SoilTranspFacAcc            ,& ! out,   accumulated soil water transpiration factor (0 to 1)
              SoilMatPotential       => noahmp%water%state%SoilMatPotential  & ! out,   soil matrix potential [m]
             )
! ----------------------------------------------------------------------

    ! soil moisture factor controlling stomatal resistance and evapotranspiration
    MPE   = 1.0e-6
    SoilTranspFacAcc = 0.0

    ! only for soil point
    if ( SurfaceType ==1 ) then
       do IZ = 1, NROOT
          if ( OptSoilWaterTranspiration == 1 ) then  ! Noah
             GX = (SoilLiqWater(IZ) - SMCWLT(IZ)) / (SMCREF(IZ) - SMCWLT(IZ))
          endif
          if ( OptSoilWaterTranspiration == 2 ) then  ! CLM
             SoilMatPotential(IZ) = max( PSIWLT, -PSISAT(IZ) * (max(0.01,SoilLiqWater(IZ))/SMCMAX(IZ)) ** (-BEXP(IZ)) )
             GX      = (1.0 - SoilMatPotential(IZ)/PSIWLT) / (1.0 + PSISAT(IZ)/PSIWLT)
          endif
          if ( OptSoilWaterTranspiration == 3 ) then  ! SSiB
             SoilMatPotential(IZ) = max( PSIWLT, -PSISAT(IZ) * (max(0.01,SoilLiqWater(IZ))/SMCMAX(IZ)) ** (-BEXP(IZ)) )
             GX      = 1.0 - exp( -5.8 * (log(PSIWLT/SoilMatPotential(IZ))) )
          endif
          GX = min( 1.0, max(0.0,GX) )

          SoilTranspFac(IZ) = max( MPE, ThicknessSnowSoilLayer(IZ) / (-DepthSoilLayer(NROOT)) * GX )
          SoilTranspFacAcc      = SoilTranspFacAcc + SoilTranspFac(IZ)
       enddo

       SoilTranspFacAcc = max( MPE, SoilTranspFacAcc )
       SoilTranspFac(1:NROOT) = SoilTranspFac(1:NROOT) / SoilTranspFacAcc
    endif

    end associate

  end subroutine SoilWaterTranspiration

end module SoilWaterTranspirationMod
