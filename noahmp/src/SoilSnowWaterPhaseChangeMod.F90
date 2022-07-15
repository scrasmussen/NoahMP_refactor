module SoilSnowWaterPhaseChangeMod

!!! Compute the phase change (melting/freezing) of snow water and soil water

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilWaterSupercoolLiquidMod, only : SoilWaterSupercoolLiquid

  implicit none

contains

  subroutine SoilSnowWaterPhaseChange(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: PHASECHANGE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                :: J         ! do loop index
    real(kind=kind_noahmp) :: HEATR     ! energy residual or loss after melting/freezing
    real(kind=kind_noahmp) :: TEMP1     ! temporary variables [kg/m2]
    real(kind=kind_noahmp) :: PROPOR    ! temporary variables 
    real(kind=kind_noahmp) :: SMP       ! frozen water potential (mm)
    real(kind=kind_noahmp) :: XMF       ! total latent heat of phase change
    real(kind=kind_noahmp), allocatable, dimension(:) :: HM        ! energy residual [w/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: XM        ! melting or freezing water [kg/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: WMASS0    ! temporary water mass
    real(kind=kind_noahmp), allocatable, dimension(:) :: WICE0     ! temporary ice content
    real(kind=kind_noahmp), allocatable, dimension(:) :: WLIQ0     ! temporary liquid content
    real(kind=kind_noahmp), allocatable, dimension(:) :: MICE      ! soil/snow ice mass [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MLIQ      ! soil/snow liquid water mass [mm]

! --------------------------------------------------------------------
    associate(                                                        &
              OptSoilSupercoolWater => noahmp%config%nmlist%OptSoilSupercoolWater ,& ! in,    options for soil supercooled liquid water
              NumSoilLayer          => noahmp%config%domain%NumSoilLayer          ,& ! in,    number of soil layers
              NumSnowLayerMax       => noahmp%config%domain%NumSnowLayerMax       ,& ! in,    maximum number of snow layers
              NumSnowLayerNeg       => noahmp%config%domain%NumSnowLayerNeg       ,& ! in,    actual number of snow layers (negative)
              MainTimeStep          => noahmp%config%domain%MainTimeStep          ,& ! in,    main noahmp timestep (s)
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,    saturated soil matric potential (m)
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,    saturated value of soil moisture [m3/m3]
              FACT            => noahmp%energy%state%FACT            ,& ! in,    energy factor for soil & snow phase change
              STC             => noahmp%energy%state%STC             ,& ! inout, snow and soil layer temperature [K]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! inout, total soil moisture [m3/m3]
              SnowIce           => noahmp%water%state%SnowIce            ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater            ,& ! inout, snow layer liquid water [mm]
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout, snow depth [m]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout, snow water equivalent [mm]
              IndexPhaseChange           => noahmp%water%state%IndexPhaseChange            ,& ! out,   phase change index [0-none;1-melt;2-refreeze]
              SoilSupercoolWater       => noahmp%water%state%SoilSupercoolWater        ,& ! out,   supercooled water in soil (kg/m2)
              QMELT           => noahmp%water%flux%QMELT             ,& ! out,   snowmelt rate [mm/s]
              PondSfcThinSnwMelt         => noahmp%water%state%PondSfcThinSnwMelt           & ! out,  surface ponding [mm] from snowmelt when thin snow has no layer
             )
! ----------------------------------------------------------------------

    !--- Initialization
    allocate( HM     (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( XM     (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( WMASS0 (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( WICE0  (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( WLIQ0  (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( MICE   (-NumSnowLayerMax+1:NumSoilLayer) )
    allocate( MLIQ   (-NumSnowLayerMax+1:NumSoilLayer) )
    QMELT   = 0.0
    PondSfcThinSnwMelt = 0.0
    XMF     = 0.0
    ! supercooled water content
    do J = -NumSnowLayerMax+1, NumSoilLayer 
         SoilSupercoolWater(J) = 0.0
    enddo
    ! snow layer water mass
    do J = NumSnowLayerNeg+1, 0
       MICE(J) = SnowIce(J)
       MLIQ(J) = SnowLiqWater(J)
    enddo
    ! soil layer water mass
    do J = 1, NumSoilLayer
       MLIQ(J) = SoilLiqWater(J) * ThicknessSnowSoilLayer(J) * 1000.0
       MICE(J) = (SoilMoisture(J) - SoilLiqWater(J)) * ThicknessSnowSoilLayer(J) * 1000.0
    enddo
    ! other required variables
    do J = NumSnowLayerNeg+1, NumSoilLayer
       IndexPhaseChange(J)  = 0
       HM(J)     = 0.0
       XM(J)     = 0.0
       WICE0(J)  = MICE(J)
       WLIQ0(J)  = MLIQ(J)
       WMASS0(J) = MICE(J) + MLIQ(J)
    enddo

    !--- compute soil supercool water content
    if ( SurfaceType == 1 ) then ! land points
       do J = 1, NumSoilLayer
          if ( OptSoilSupercoolWater == 1 ) then
             if ( STC(J) < ConstFreezePoint ) then
                SMP          = ConstLatHeatFusion * (ConstFreezePoint - STC(J)) / (ConstGravityAcc * STC(J)) !(m)
                SoilSupercoolWater(J) = SMCMAX(J) * (SMP / PSISAT(J)) ** (-1.0 / BEXP(J))
                SoilSupercoolWater(J) = SoilSupercoolWater(J) * ThicknessSnowSoilLayer(J) * 1000.0        !(mm)
             endif
          endif
          if ( OptSoilSupercoolWater == 2 ) then
               call SoilWaterSupercoolLiquid(noahmp, J, SoilSupercoolWater(J), STC(J), SoilMoisture(J), SoilLiqWater(J))
               SoilSupercoolWater(J) = SoilSupercoolWater(J) * ThicknessSnowSoilLayer(J) * 1000.0        !(mm)
          endif
       enddo
    endif

    !--- determine melting or freezing state
    do J = NumSnowLayerNeg+1, NumSoilLayer
       if ( (MICE(J) > 0.0) .and. (STC(J) >= ConstFreezePoint) ) then
          IndexPhaseChange(J) = 1  ! melting
       endif
       if ( (MLIQ(J) > SoilSupercoolWater(J)) .and. (STC(J) < ConstFreezePoint) ) then
          IndexPhaseChange(J) = 2  ! freezing
       endif
       ! If snow exists, but its thickness is not enough to create a layer
       if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (J == 1) ) then
          if ( STC(J) >= ConstFreezePoint ) then
             IndexPhaseChange(J) = 1
          endif
       endif
    enddo

    !--- Calculate the energy surplus and loss for melting and freezing
    do J = NumSnowLayerNeg+1, NumSoilLayer
       if ( IndexPhaseChange(J) > 0 ) then
          HM(J)  = (STC(J) - ConstFreezePoint) / FACT(J)
          STC(J) = ConstFreezePoint
       endif
       if ( (IndexPhaseChange(J) == 1) .and. (HM(J) < 0.0) ) then
          HM(J)    = 0.0
          IndexPhaseChange(J) = 0
       endif
       if ( (IndexPhaseChange(J) == 2) .and. (HM(J) > 0.0) ) then
          HM(J)    = 0.0
          IndexPhaseChange(J) = 0
       endif
       XM(J) = HM(J) * MainTimeStep / ConstLatHeatFusion
    enddo

    !--- The rate of melting and freezing for snow without a layer, needs more work.
    if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (XM(1) > 0.0) ) then
       TEMP1  = SnowWaterEquiv
       SnowWaterEquiv  = max( 0.0, TEMP1-XM(1) )
       PROPOR = SnowWaterEquiv / TEMP1
       SnowDepth  = max( 0.0, PROPOR*SnowDepth )
       SnowDepth  = min( max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0 )  ! limit adjustment to a reasonable density
       HEATR  = HM(1) - ConstLatHeatFusion * (TEMP1 - SnowWaterEquiv) / MainTimeStep
       if ( HEATR > 0.0 ) then
          XM(1) = HEATR * MainTimeStep / ConstLatHeatFusion
          HM(1) = HEATR
       else
          XM(1) = 0.0
          HM(1) = 0.0
       endif
       QMELT   = max( 0.0, (TEMP1-SnowWaterEquiv) ) / MainTimeStep
       XMF     = ConstLatHeatFusion * QMELT
       PondSfcThinSnwMelt = TEMP1 - SnowWaterEquiv
    endif

    ! The rate of melting and freezing for multi-layer snow and soil
    do J = NumSnowLayerNeg+1, NumSoilLayer
       if ( (IndexPhaseChange(J) > 0) .and. (abs(HM(J)) > 0.0) ) then
          HEATR = 0.0
          if ( XM(J) > 0.0 ) then
             MICE(J) = max( 0.0, WICE0(J)-XM(J) )
             HEATR   = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
          elseif ( XM(J) < 0.0 ) then
             if ( J <= 0 ) then  ! snow layer
                MICE(J) = min( WMASS0(J), WICE0(J)-XM(J) )
             else    ! soil layer
                if ( WMASS0(J) < SoilSupercoolWater(J) ) then
                   MICE(J) = 0.0
                else
                   MICE(J) = min( WMASS0(J)-SoilSupercoolWater(J), WICE0(J)-XM(J) )
                   MICE(J) = max( MICE(J), 0.0 )
                endif
             endif
             HEATR = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
          endif
          MLIQ(J) = max( 0.0, WMASS0(J)-MICE(J) ) ! update liquid water mass

          ! update soil/snow temperature and energy surplus/loss
          if ( abs(HEATR) > 0.0 ) then
             STC(J) = STC(J) + FACT(J) * HEATR
             if ( J <= 0 ) then  ! snow
                if ( (MLIQ(J)*MICE(J)) > 0.0 ) STC(J) = ConstFreezePoint
                if ( MICE(J) == 0.0 ) then         ! BARLAGE
                   STC(J)  = ConstFreezePoint      ! BARLAGE
                   HM(J+1) = HM(J+1) + HEATR       ! BARLAGE
                   XM(J+1) = HM(J+1) * MainTimeStep / ConstLatHeatFusion   ! BARLAGE
                endif
             endif
          endif
          XMF = XMF + ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
          ! snow melting rate
          if ( J < 1 ) then
             QMELT = QMELT + max( 0.0, (WICE0(J)-MICE(J)) ) / MainTimeStep
          endif
       endif
    enddo

    !--- update snow and soil ice and liquid content
    do J = NumSnowLayerNeg+1, 0     ! snow
       SnowLiqWater(J) = MLIQ(J)
       SnowIce(J) = MICE(J)
    enddo
    do J = 1, NumSoilLayer       ! soil
       SoilLiqWater(J) = MLIQ(J) / (1000.0 * ThicknessSnowSoilLayer(J))
       SoilMoisture(J)  = (MLIQ(J) + MICE(J)) / (1000.0 * ThicknessSnowSoilLayer(J))
    enddo

    end associate

  end subroutine SoilSnowWaterPhaseChange

end module SoilSnowWaterPhaseChangeMod
