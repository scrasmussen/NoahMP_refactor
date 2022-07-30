module GlacierPhaseChangeMod

!!! Compute the phase change (melting/freezing) of snow and glacier ice

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GlacierPhaseChange(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: PHASECHANGE_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                :: J, K      ! do loop index
    real(kind=kind_noahmp) :: TEMP1     ! temporary variables [kg/m2]
    real(kind=kind_noahmp) :: PROPOR    ! temporary variables 
    real(kind=kind_noahmp) :: XMF       ! total latent heat of phase change
    real(kind=kind_noahmp), allocatable, dimension(:) :: HM        ! energy residual [w/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: XM        ! melting or freezing water [kg/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: WMASS0    ! temporary water mass
    real(kind=kind_noahmp), allocatable, dimension(:) :: WICE0     ! temporary ice content
    real(kind=kind_noahmp), allocatable, dimension(:) :: WLIQ0     ! temporary liquid content
    real(kind=kind_noahmp), allocatable, dimension(:) :: MICE      ! soil/snow ice mass [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MLIQ      ! soil/snow liquid water mass [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: HEATR     ! energy residual or loss after melting/freezing

! --------------------------------------------------------------------
    associate(                                                        &
              OptGlacierTreatment => noahmp%config%nmlist%OptGlacierTreatment ,& ! in,    options for glacier treatment
              NumSoilLayer        => noahmp%config%domain%NumSoilLayer        ,& ! in,    number of soil layers
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax,& ! in,    maximum number of snow layers
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,    actual number of snow layers (negative)
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers [m]
              PhaseChgFacSoilSnow            => noahmp%energy%state%PhaseChgFacSoilSnow            ,& ! in,    energy factor for soil & snow phase change
              TemperatureSoilSnow             => noahmp%energy%state%TemperatureSoilSnow             ,& ! inout, snow and soil layer temperature [K]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! inout, total soil moisture [m3/m3]
              SnowIce           => noahmp%water%state%SnowIce            ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater            ,& ! inout, snow layer liquid water [mm]
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout, snow depth [m]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout, snow water equivalent [mm]
              IndexPhaseChange           => noahmp%water%state%IndexPhaseChange            ,& ! out,   phase change index [0-none;1-melt;2-refreeze]
              MeltGroundSnow           => noahmp%water%flux%MeltGroundSnow             ,& ! out,   ground snowmelt rate [mm/s]
              PondSfcThinSnwMelt         => noahmp%water%state%PondSfcThinSnwMelt           & ! out,   surface ponding [mm] from snowmelt when thin snow has no layer
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
    allocate( HEATR  (-NumSnowLayerMax+1:NumSoilLayer) )
    MeltGroundSnow   = 0.0
    PondSfcThinSnwMelt = 0.0
    XMF     = 0.0

    !--- treat snowpack over glacier ice first

    ! snow layer water mass
    do J = NumSnowLayerNeg+1, 0
       MICE(J) = SnowIce(J)
       MLIQ(J) = SnowLiqWater(J)
    enddo

    ! other required variables
    do J = NumSnowLayerNeg+1, 0
       IndexPhaseChange(J)  = 0
       HM(J)     = 0.0
       XM(J)     = 0.0
       HEATR(J)  = 0.0
       WICE0(J)  = MICE(J)
       WLIQ0(J)  = MLIQ(J)
       WMASS0(J) = MICE(J) + MLIQ(J)
    enddo

    ! determine melting or freezing state
    do J = NumSnowLayerNeg+1, 0
       if ( (MICE(J) > 0.0) .and. (TemperatureSoilSnow(J) >= ConstFreezePoint) ) then
          IndexPhaseChange(J) = 1  ! melting
       endif
       if ( (MLIQ(J) > 0.0) .and. (TemperatureSoilSnow(J) < ConstFreezePoint) ) then
          IndexPhaseChange(J) = 2  ! freezing
       endif
    enddo

    ! Calculate the energy surplus and loss for melting and freezing
    do J = NumSnowLayerNeg+1, 0
       if ( IndexPhaseChange(J) > 0 ) then
          HM(J)  = (TemperatureSoilSnow(J) - ConstFreezePoint) / PhaseChgFacSoilSnow(J)
          TemperatureSoilSnow(J) = ConstFreezePoint
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

    ! The rate of melting and freezing for snow without a layer, needs more work.
    if ( OptGlacierTreatment == 2 ) then
       if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (TemperatureSoilSnow(1) > ConstFreezePoint) ) then
          HM(1)    = (TemperatureSoilSnow(1) - ConstFreezePoint) / PhaseChgFacSoilSnow(1)                  ! available heat
          TemperatureSoilSnow(1)   = ConstFreezePoint                                       ! set T to freezing
          XM(1)    = HM(1) * MainTimeStep / ConstLatHeatFusion                          ! total snow melt possible
          TEMP1    = SnowWaterEquiv
          SnowWaterEquiv    = max( 0.0, TEMP1-XM(1) )                    ! snow remaining
          PROPOR   = SnowWaterEquiv / TEMP1                              ! fraction melted
          SnowDepth    = max( 0.0, PROPOR*SnowDepth )                   ! new snow height
          SnowDepth    = min( max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0 )  ! limit adjustment to a reasonable density
          HEATR(1) = HM(1) - ConstLatHeatFusion * (TEMP1 - SnowWaterEquiv) / MainTimeStep        ! excess heat
          if ( HEATR(1) > 0.0 ) then
             XM(1) = HEATR(1) * MainTimeStep / ConstLatHeatFusion
             TemperatureSoilSnow(1)= TemperatureSoilSnow(1) + PhaseChgFacSoilSnow(1) * HEATR(1)                ! re-heat ice
          else
             XM(1) = 0.0
             HM(1) = 0.0
          endif
          MeltGroundSnow    = max( 0.0, (TEMP1-SnowWaterEquiv) ) / MainTimeStep             ! melted snow rate
          XMF      = ConstLatHeatFusion * MeltGroundSnow                               ! melted snow energy
          PondSfcThinSnwMelt  = TEMP1 - SnowWaterEquiv                              ! melt water
       endif
    endif ! OptGlacierTreatment==2

    ! The rate of melting and freezing for multi-layer snow
    do J = NumSnowLayerNeg+1, 0
       if ( (IndexPhaseChange(J) > 0) .and. (abs(HM(J)) > 0.0) ) then
          HEATR(J) = 0.0
          if ( XM(J) > 0.0 ) then
             MICE(J)  = max( 0.0, WICE0(J)-XM(J) )
             HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
          elseif ( XM(J) < 0.0 ) then
             MICE(J)  = min( WMASS0(J), WICE0(J)-XM(J) )
             HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
          endif
          MLIQ(J) = max( 0.0, WMASS0(J)-MICE(J) )               ! update liquid water mass

          ! update snow temperature and energy surplus/loss
          if ( abs(HEATR(J)) > 0.0 ) then
             TemperatureSoilSnow(J) = TemperatureSoilSnow(J) + PhaseChgFacSoilSnow(J) * HEATR(J)
             if ( (MLIQ(J)*MICE(J)) > 0.0 ) TemperatureSoilSnow(J) = ConstFreezePoint
          endif
          XMF = XMF + ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep

          ! snow melting rate
          MeltGroundSnow = MeltGroundSnow + max( 0.0, (WICE0(J)-MICE(J)) ) / MainTimeStep
       endif
    enddo

    !---- glacier ice layer treatment

    if ( OptGlacierTreatment == 1 ) then

       ! ice layer water mass
       do J = 1, NumSoilLayer
          MLIQ(J) = SoilLiqWater(J) * ThicknessSnowSoilLayer(J) * 1000.0
          MICE(J) = (SoilMoisture(J) - SoilLiqWater(J)) * ThicknessSnowSoilLayer(J) * 1000.0
       enddo

       ! other required variables
       do J = 1, NumSoilLayer
          IndexPhaseChange(J)  = 0
          HM(J)     = 0.0
          XM(J)     = 0.0
          HEATR(J)  = 0.0
          WICE0(J)  = MICE(J)
          WLIQ0(J)  = MLIQ(J)
          WMASS0(J) = MICE(J) + MLIQ(J)
       enddo

       ! determine melting or freezing state
       do J = 1, NumSoilLayer
          if ( (MICE(J) > 0.0) .and. (TemperatureSoilSnow(J) >= ConstFreezePoint) ) then
             IndexPhaseChange(J) = 1  ! melting
          endif
          if ( (MLIQ(J) > 0.0) .and. (TemperatureSoilSnow(J) < ConstFreezePoint) ) then
             IndexPhaseChange(J) = 2  ! freezing
          endif
          ! If snow exists, but its thickness is not enough to create a layer
          if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (J == 1) ) then
             if ( TemperatureSoilSnow(J) >= ConstFreezePoint ) then
                IndexPhaseChange(J) = 1
             endif
          endif
       enddo

       ! Calculate the energy surplus and loss for melting and freezing
       do J = 1, NumSoilLayer
          if ( IndexPhaseChange(J) > 0 ) then
             HM(J)  = (TemperatureSoilSnow(J) - ConstFreezePoint) / PhaseChgFacSoilSnow(J)
             TemperatureSoilSnow(J) = ConstFreezePoint
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

       ! The rate of melting and freezing for snow without a layer, needs more work.
       if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (XM(1) > 0.0) ) then
          TEMP1    = SnowWaterEquiv
          SnowWaterEquiv    = max( 0.0, TEMP1-XM(1) )
          PROPOR   = SnowWaterEquiv / TEMP1
          SnowDepth    = max( 0.0, PROPOR*SnowDepth )
          SnowDepth    = min( max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0 )  ! limit adjustment to a reasonable density
          HEATR(1) = HM(1) - ConstLatHeatFusion * (TEMP1 - SnowWaterEquiv) / MainTimeStep
          if ( HEATR(1) > 0.0 ) then
             XM(1)    = HEATR(1) * MainTimeStep / ConstLatHeatFusion
             HM(1)    = HEATR(1)
             IndexPhaseChange(1) = 1
          else
             XM(1)    = 0.0
             HM(1)    = 0.0
             IndexPhaseChange(1) = 0
          endif
          MeltGroundSnow   = max( 0.0, (TEMP1-SnowWaterEquiv) ) / MainTimeStep
          XMF     = ConstLatHeatFusion * MeltGroundSnow
          PondSfcThinSnwMelt = TEMP1 - SnowWaterEquiv
       endif

       ! The rate of melting and freezing for glacier ice
       do J = 1, NumSoilLayer
          if ( (IndexPhaseChange(J) > 0) .and. (abs(HM(J)) > 0.0) ) then
             HEATR(J) = 0.0
             if ( XM(J) > 0.0 ) then
                MICE(J)  = max( 0.0, WICE0(J)-XM(J) )
                HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
             elseif ( XM(J) < 0.0 ) then
                MICE(J)  = min( WMASS0(J), WICE0(J)-XM(J) )
                HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
             endif
             MLIQ(J) = max( 0.0, WMASS0(J)-MICE(J) ) ! update liquid water mass

             ! update ice temperature and energy surplus/loss
             if ( abs(HEATR(J)) > 0.0 ) then
                TemperatureSoilSnow(J) = TemperatureSoilSnow(J) + PhaseChgFacSoilSnow(J) * HEATR(J)
             endif
             XMF = XMF + ConstLatHeatFusion * (WICE0(J) - MICE(J)) / MainTimeStep
          endif
       enddo
       HEATR = 0.0
       XM    = 0.0

       !--- Deal with residuals in ice/soil

       ! first remove excess heat by reducing layer temperature
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) > ConstFreezePoint) .and. any(TemperatureSoilSnow(1:NumSoilLayer) < ConstFreezePoint) ) then
          do J = 1, NumSoilLayer
             if ( TemperatureSoilSnow(J) > ConstFreezePoint ) then
                HEATR(J) = (TemperatureSoilSnow(J) - ConstFreezePoint) / PhaseChgFacSoilSnow(J)
                do K = 1, NumSoilLayer
                   if ( (J /= K) .and. (TemperatureSoilSnow(K) < ConstFreezePoint) .and. (HEATR(J) > 0.1) ) then
                      HEATR(K) = (TemperatureSoilSnow(K) - ConstFreezePoint) / PhaseChgFacSoilSnow(K)
                      if ( abs(HEATR(K)) > HEATR(J) ) then ! LAYER ABSORBS ALL
                         HEATR(K) = HEATR(K) + HEATR(J)
                         TemperatureSoilSnow(K)   = ConstFreezePoint + HEATR(K) * PhaseChgFacSoilSnow(K)
                         HEATR(J) = 0.0
                      else
                         HEATR(J) = HEATR(J) + HEATR(K)
                         HEATR(K) = 0.0
                         TemperatureSoilSnow(K)   = ConstFreezePoint
                      endif
                   endif
                enddo
                TemperatureSoilSnow(J) = ConstFreezePoint + HEATR(J) * PhaseChgFacSoilSnow(J)
             endif
          enddo
       endif

       ! now remove excess cold by increasing temperture (may not be necessary with above loop)
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) > ConstFreezePoint) .and. any(TemperatureSoilSnow(1:NumSoilLayer) < ConstFreezePoint) ) then
          do J = 1, NumSoilLayer
             if ( TemperatureSoilSnow(J) < ConstFreezePoint ) then
                HEATR(J) = (TemperatureSoilSnow(J) - ConstFreezePoint) / PhaseChgFacSoilSnow(J)
                do K = 1, NumSoilLayer
                   if ( (J /= K) .and. (TemperatureSoilSnow(K) > ConstFreezePoint) .and. (HEATR(J) < -0.1) ) then
                      HEATR(K) = (TemperatureSoilSnow(K) - ConstFreezePoint) / PhaseChgFacSoilSnow(K)
                      if ( HEATR(K) > abs(HEATR(J)) ) then  ! LAYER ABSORBS ALL
                         HEATR(K) = HEATR(K) + HEATR(J)
                         TemperatureSoilSnow(K)   = ConstFreezePoint + HEATR(K) * PhaseChgFacSoilSnow(K)
                         HEATR(J) = 0.0
                      else
                         HEATR(J) = HEATR(J) + HEATR(K)
                         HEATR(K) = 0.0
                         TemperatureSoilSnow(K)   = ConstFreezePoint
                      endif
                   endif
                enddo
                TemperatureSoilSnow(J) = ConstFreezePoint + HEATR(J) * PhaseChgFacSoilSnow(J)
             endif
          enddo
       endif

       ! now remove excess heat by melting ice
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) > ConstFreezePoint) .and. any(MICE(1:NumSoilLayer) > 0.0) ) then
          do J = 1, NumSoilLayer
             if ( TemperatureSoilSnow(J) > ConstFreezePoint ) then
                HEATR(J) = (TemperatureSoilSnow(J) - ConstFreezePoint) / PhaseChgFacSoilSnow(J)
                XM(J)    = HEATR(J) * MainTimeStep / ConstLatHeatFusion
                do K = 1, NumSoilLayer
                   if ( (J /= K) .and. (MICE(K) > 0.0) .and. (XM(J) > 0.1) ) then
                      if ( MICE(K) > XM(J) ) then  ! LAYER ABSORBS ALL
                         MICE(K) = MICE(K) - XM(J)
                         XMF     = XMF + ConstLatHeatFusion * XM(J)/MainTimeStep
                         TemperatureSoilSnow(K)  = ConstFreezePoint
                         XM(J)   = 0.0
                      else
                         XM(J)   = XM(J) - MICE(K)
                         XMF     = XMF + ConstLatHeatFusion * MICE(K) / MainTimeStep
                         MICE(K) = 0.0
                         TemperatureSoilSnow(K)  = ConstFreezePoint
                      endif
                      MLIQ(K) = max( 0.0, WMASS0(K)-MICE(K) )
                   endif
                enddo
                HEATR(J) = XM(J) * ConstLatHeatFusion / MainTimeStep
                TemperatureSoilSnow(J)   = ConstFreezePoint + HEATR(J) * PhaseChgFacSoilSnow(J)
             endif
          enddo
       endif

       ! snow remove excess cold by refreezing liquid (may not be necessary with above loop)
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) < ConstFreezePoint) .and. any(MLIQ(1:NumSoilLayer) > 0.0) ) then
          do J = 1, NumSoilLayer
             if ( TemperatureSoilSnow(J) < ConstFreezePoint ) then
                HEATR(J) = (TemperatureSoilSnow(J) - ConstFreezePoint) / PhaseChgFacSoilSnow(J)
                XM(J)    = HEATR(J) * MainTimeStep / ConstLatHeatFusion
                do K = 1, NumSoilLayer
                   if ( (J /= K) .and. (MLIQ(K) > 0.0) .and. (XM(J) < -0.1) ) then
                      if ( MLIQ(K) > abs(XM(J)) ) then  ! LAYER ABSORBS ALL
                         MICE(K) = MICE(K) - XM(J)
                         XMF     = XMF + ConstLatHeatFusion * XM(J) / MainTimeStep
                         TemperatureSoilSnow(K)  = ConstFreezePoint
                         XM(J)   = 0.0
                      else
                         XM(J)   = XM(J) + MLIQ(K)
                         XMF     = XMF - ConstLatHeatFusion * MLIQ(K) / MainTimeStep
                         MICE(K) = WMASS0(K)
                         TemperatureSoilSnow(K)  = ConstFreezePoint
                      endif
                      MLIQ(K) = max( 0.0, WMASS0(K)-MICE(K) )
                   endif
                enddo
                HEATR(J) = XM(J) * ConstLatHeatFusion / MainTimeStep
                TemperatureSoilSnow(J)   = ConstFreezePoint + HEATR(J) * PhaseChgFacSoilSnow(J)
             endif
          enddo
       endif

    endif ! OptGlacierTreatment==1

    !--- update snow and soil ice and liquid content
    do J = NumSnowLayerNeg+1, 0     ! snow
       SnowLiqWater(J) = MLIQ(J)
       SnowIce(J) = MICE(J)
    enddo
    do J = 1, NumSoilLayer       ! glacier ice
       if ( OptGlacierTreatment == 1 ) then
          SoilLiqWater(J) = MLIQ(J) / (1000.0 * ThicknessSnowSoilLayer(J))
          SoilLiqWater(J) = max( 0.0, min(1.0,SoilLiqWater(J)) )
       elseif ( OptGlacierTreatment == 2 ) then
          SoilLiqWater(J) = 0.0             ! ice, assume all frozen forever
       endif
       SoilMoisture(J) = 1.0
    enddo

    end associate

  end subroutine GlacierPhaseChange

end module GlacierPhaseChangeMod
