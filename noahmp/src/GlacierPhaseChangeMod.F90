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
              OPT_GLA         => noahmp%config%nmlist%OPT_GLA        ,& ! in,    options for glacier treatment
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,    maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,    thickness of snow/soil layers (m)
              FACT            => noahmp%energy%state%FACT            ,& ! in,    energy factor for soil & snow phase change
              STC             => noahmp%energy%state%STC             ,& ! inout, snow and soil layer temperature [K]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout, soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout, total soil moisture [m3/m3]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout, snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout, snow layer liquid water [mm]
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout, snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout, snow water equivalent [mm]
              IMELT           => noahmp%water%state%IMELT            ,& ! out,   phase change index [0-none;1-melt;2-refreeze]
              QMELT           => noahmp%water%flux%QMELT             ,& ! out,   snowmelt rate [mm/s]
              PONDING         => noahmp%water%state%PONDING           & ! out,   melting water from snow when there is no layer (mm)
             )
! ----------------------------------------------------------------------

    !--- Initialization
    allocate( HM     (-NSNOW+1:NSOIL) )
    allocate( XM     (-NSNOW+1:NSOIL) )
    allocate( WMASS0 (-NSNOW+1:NSOIL) )
    allocate( WICE0  (-NSNOW+1:NSOIL) )
    allocate( WLIQ0  (-NSNOW+1:NSOIL) )
    allocate( MICE   (-NSNOW+1:NSOIL) )
    allocate( MLIQ   (-NSNOW+1:NSOIL) )
    allocate( HEATR  (-NSNOW+1:NSOIL) )
    QMELT   = 0.0
    PONDING = 0.0
    XMF     = 0.0

    !--- treat snowpack over glacier ice first

    ! snow layer water mass
    do J = ISNOW+1, 0
       MICE(J) = SNICE(J)
       MLIQ(J) = SNLIQ(J)
    enddo

    ! other required variables
    do J = ISNOW+1, 0
       IMELT(J)  = 0
       HM(J)     = 0.0
       XM(J)     = 0.0
       HEATR(J)  = 0.0
       WICE0(J)  = MICE(J)
       WLIQ0(J)  = MLIQ(J)
       WMASS0(J) = MICE(J) + MLIQ(J)
    enddo

    ! determine melting or freezing state
    do J = ISNOW+1, 0
       if ( (MICE(J) > 0.0) .and. (STC(J) >= ConstFreezePoint) ) then
          IMELT(J) = 1  ! melting
       endif
       if ( (MLIQ(J) > 0.0) .and. (STC(J) < ConstFreezePoint) ) then
          IMELT(J) = 2  ! freezing
       endif
    enddo

    ! Calculate the energy surplus and loss for melting and freezing
    do J = ISNOW+1, 0
       if ( IMELT(J) > 0 ) then
          HM(J)  = (STC(J) - ConstFreezePoint) / FACT(J)
          STC(J) = ConstFreezePoint
       endif
       if ( (IMELT(J) == 1) .and. (HM(J) < 0.0) ) then
          HM(J)    = 0.0
          IMELT(J) = 0
       endif
       if ( (IMELT(J) == 2) .and. (HM(J) > 0.0) ) then
          HM(J)    = 0.0
          IMELT(J) = 0
       endif
       XM(J) = HM(J) * DT / ConstLatHeatFusion
    enddo

    ! The rate of melting and freezing for snow without a layer, needs more work.
    if ( OPT_GLA == 2 ) then
       if ( (ISNOW == 0) .and. (SNEQV > 0.0) .and. (STC(1) > ConstFreezePoint) ) then
          HM(1)    = (STC(1) - ConstFreezePoint) / FACT(1)                  ! available heat
          STC(1)   = ConstFreezePoint                                       ! set T to freezing
          XM(1)    = HM(1) * DT / ConstLatHeatFusion                          ! total snow melt possible
          TEMP1    = SNEQV
          SNEQV    = max( 0.0, TEMP1-XM(1) )                    ! snow remaining
          PROPOR   = SNEQV / TEMP1                              ! fraction melted
          SNOWH    = max( 0.0, PROPOR*SNOWH )                   ! new snow height
          SNOWH    = min( max(SNOWH,SNEQV/500.0), SNEQV/50.0 )  ! limit adjustment to a reasonable density
          HEATR(1) = HM(1) - ConstLatHeatFusion * (TEMP1 - SNEQV) / DT        ! excess heat
          if ( HEATR(1) > 0.0 ) then
             XM(1) = HEATR(1) * DT / ConstLatHeatFusion
             STC(1)= STC(1) + FACT(1) * HEATR(1)                ! re-heat ice
          else
             XM(1) = 0.0
             HM(1) = 0.0
          endif
          QMELT    = max( 0.0, (TEMP1-SNEQV) ) / DT             ! melted snow rate
          XMF      = ConstLatHeatFusion * QMELT                               ! melted snow energy
          PONDING  = TEMP1 - SNEQV                              ! melt water
       endif
    endif ! OPT_GLA==2

    ! The rate of melting and freezing for multi-layer snow
    do J = ISNOW+1, 0
       if ( (IMELT(J) > 0) .and. (abs(HM(J)) > 0.0) ) then
          HEATR(J) = 0.0
          if ( XM(J) > 0.0 ) then
             MICE(J)  = max( 0.0, WICE0(J)-XM(J) )
             HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
          elseif ( XM(J) < 0.0 ) then
             MICE(J)  = min( WMASS0(J), WICE0(J)-XM(J) )
             HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
          endif
          MLIQ(J) = max( 0.0, WMASS0(J)-MICE(J) )               ! update liquid water mass

          ! update snow temperature and energy surplus/loss
          if ( abs(HEATR(J)) > 0.0 ) then
             STC(J) = STC(J) + FACT(J) * HEATR(J)
             if ( (MLIQ(J)*MICE(J)) > 0.0 ) STC(J) = ConstFreezePoint
          endif
          XMF = XMF + ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT

          ! snow melting rate
          QMELT = QMELT + max( 0.0, (WICE0(J)-MICE(J)) ) / DT
       endif
    enddo

    !---- glacier ice layer treatment

    if ( OPT_GLA == 1 ) then

       ! ice layer water mass
       do J = 1, NSOIL
          MLIQ(J) = SH2O(J) * DZSNSO(J) * 1000.0
          MICE(J) = (SMC(J) - SH2O(J)) * DZSNSO(J) * 1000.0
       enddo

       ! other required variables
       do J = 1, NSOIL
          IMELT(J)  = 0
          HM(J)     = 0.0
          XM(J)     = 0.0
          HEATR(J)  = 0.0
          WICE0(J)  = MICE(J)
          WLIQ0(J)  = MLIQ(J)
          WMASS0(J) = MICE(J) + MLIQ(J)
       enddo

       ! determine melting or freezing state
       do J = 1, NSOIL
          if ( (MICE(J) > 0.0) .and. (STC(J) >= ConstFreezePoint) ) then
             IMELT(J) = 1  ! melting
          endif
          if ( (MLIQ(J) > 0.0) .and. (STC(J) < ConstFreezePoint) ) then
             IMELT(J) = 2  ! freezing
          endif
          ! If snow exists, but its thickness is not enough to create a layer
          if ( (ISNOW == 0) .and. (SNEQV > 0.0) .and. (J == 1) ) then
             if ( STC(J) >= ConstFreezePoint ) then
                IMELT(J) = 1
             endif
          endif
       enddo

       ! Calculate the energy surplus and loss for melting and freezing
       do J = 1, NSOIL
          if ( IMELT(J) > 0 ) then
             HM(J)  = (STC(J) - ConstFreezePoint) / FACT(J)
             STC(J) = ConstFreezePoint
          endif
          if ( (IMELT(J) == 1) .and. (HM(J) < 0.0) ) then
             HM(J)    = 0.0
             IMELT(J) = 0
          endif
          if ( (IMELT(J) == 2) .and. (HM(J) > 0.0) ) then
             HM(J)    = 0.0
             IMELT(J) = 0
          endif
          XM(J) = HM(J) * DT / ConstLatHeatFusion
       enddo

       ! The rate of melting and freezing for snow without a layer, needs more work.
       if ( (ISNOW == 0) .and. (SNEQV > 0.0) .and. (XM(1) > 0.0) ) then
          TEMP1    = SNEQV
          SNEQV    = max( 0.0, TEMP1-XM(1) )
          PROPOR   = SNEQV / TEMP1
          SNOWH    = max( 0.0, PROPOR*SNOWH )
          SNOWH    = min( max(SNOWH,SNEQV/500.0), SNEQV/50.0 )  ! limit adjustment to a reasonable density
          HEATR(1) = HM(1) - ConstLatHeatFusion * (TEMP1 - SNEQV) / DT
          if ( HEATR(1) > 0.0 ) then
             XM(1)    = HEATR(1) * DT / ConstLatHeatFusion
             HM(1)    = HEATR(1)
             IMELT(1) = 1
          else
             XM(1)    = 0.0
             HM(1)    = 0.0
             IMELT(1) = 0
          endif
          QMELT   = max( 0.0, (TEMP1-SNEQV) ) / DT
          XMF     = ConstLatHeatFusion * QMELT
          PONDING = TEMP1 - SNEQV
       endif

       ! The rate of melting and freezing for glacier ice
       do J = 1, NSOIL
          if ( (IMELT(J) > 0) .and. (abs(HM(J)) > 0.0) ) then
             HEATR(J) = 0.0
             if ( XM(J) > 0.0 ) then
                MICE(J)  = max( 0.0, WICE0(J)-XM(J) )
                HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
             elseif ( XM(J) < 0.0 ) then
                MICE(J)  = min( WMASS0(J), WICE0(J)-XM(J) )
                HEATR(J) = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
             endif
             MLIQ(J) = max( 0.0, WMASS0(J)-MICE(J) ) ! update liquid water mass

             ! update ice temperature and energy surplus/loss
             if ( abs(HEATR(J)) > 0.0 ) then
                STC(J) = STC(J) + FACT(J) * HEATR(J)
             endif
             XMF = XMF + ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
          endif
       enddo
       HEATR = 0.0
       XM    = 0.0

       !--- Deal with residuals in ice/soil

       ! first remove excess heat by reducing layer temperature
       if ( any(STC(1:NSOIL) > ConstFreezePoint) .and. any(STC(1:NSOIL) < ConstFreezePoint) ) then
          do J = 1, NSOIL
             if ( STC(J) > ConstFreezePoint ) then
                HEATR(J) = (STC(J) - ConstFreezePoint) / FACT(J)
                do K = 1, NSOIL
                   if ( (J /= K) .and. (STC(K) < ConstFreezePoint) .and. (HEATR(J) > 0.1) ) then
                      HEATR(K) = (STC(K) - ConstFreezePoint) / FACT(K)
                      if ( abs(HEATR(K)) > HEATR(J) ) then ! LAYER ABSORBS ALL
                         HEATR(K) = HEATR(K) + HEATR(J)
                         STC(K)   = ConstFreezePoint + HEATR(K) * FACT(K)
                         HEATR(J) = 0.0
                      else
                         HEATR(J) = HEATR(J) + HEATR(K)
                         HEATR(K) = 0.0
                         STC(K)   = ConstFreezePoint
                      endif
                   endif
                enddo
                STC(J) = ConstFreezePoint + HEATR(J) * FACT(J)
             endif
          enddo
       endif

       ! now remove excess cold by increasing temperture (may not be necessary with above loop)
       if ( any(STC(1:NSOIL) > ConstFreezePoint) .and. any(STC(1:NSOIL) < ConstFreezePoint) ) then
          do J = 1, NSOIL
             if ( STC(J) < ConstFreezePoint ) then
                HEATR(J) = (STC(J) - ConstFreezePoint) / FACT(J)
                do K = 1, NSOIL
                   if ( (J /= K) .and. (STC(K) > ConstFreezePoint) .and. (HEATR(J) < -0.1) ) then
                      HEATR(K) = (STC(K) - ConstFreezePoint) / FACT(K)
                      if ( HEATR(K) > abs(HEATR(J)) ) then  ! LAYER ABSORBS ALL
                         HEATR(K) = HEATR(K) + HEATR(J)
                         STC(K)   = ConstFreezePoint + HEATR(K) * FACT(K)
                         HEATR(J) = 0.0
                      else
                         HEATR(J) = HEATR(J) + HEATR(K)
                         HEATR(K) = 0.0
                         STC(K)   = ConstFreezePoint
                      endif
                   endif
                enddo
                STC(J) = ConstFreezePoint + HEATR(J) * FACT(J)
             endif
          enddo
       endif

       ! now remove excess heat by melting ice
       if ( any(STC(1:NSOIL) > ConstFreezePoint) .and. any(MICE(1:NSOIL) > 0.0) ) then
          do J = 1, NSOIL
             if ( STC(J) > ConstFreezePoint ) then
                HEATR(J) = (STC(J) - ConstFreezePoint) / FACT(J)
                XM(J)    = HEATR(J) * DT / ConstLatHeatFusion
                do K = 1, NSOIL
                   if ( (J /= K) .and. (MICE(K) > 0.0) .and. (XM(J) > 0.1) ) then
                      if ( MICE(K) > XM(J) ) then  ! LAYER ABSORBS ALL
                         MICE(K) = MICE(K) - XM(J)
                         XMF     = XMF + ConstLatHeatFusion * XM(J)/DT
                         STC(K)  = ConstFreezePoint
                         XM(J)   = 0.0
                      else
                         XM(J)   = XM(J) - MICE(K)
                         XMF     = XMF + ConstLatHeatFusion * MICE(K) / DT
                         MICE(K) = 0.0
                         STC(K)  = ConstFreezePoint
                      endif
                      MLIQ(K) = max( 0.0, WMASS0(K)-MICE(K) )
                   endif
                enddo
                HEATR(J) = XM(J) * ConstLatHeatFusion / DT
                STC(J)   = ConstFreezePoint + HEATR(J) * FACT(J)
             endif
          enddo
       endif

       ! snow remove excess cold by refreezing liquid (may not be necessary with above loop)
       if ( any(STC(1:NSOIL) < ConstFreezePoint) .and. any(MLIQ(1:NSOIL) > 0.0) ) then
          do J = 1, NSOIL
             if ( STC(J) < ConstFreezePoint ) then
                HEATR(J) = (STC(J) - ConstFreezePoint) / FACT(J)
                XM(J)    = HEATR(J) * DT / ConstLatHeatFusion
                do K = 1, NSOIL
                   if ( (J /= K) .and. (MLIQ(K) > 0.0) .and. (XM(J) < -0.1) ) then
                      if ( MLIQ(K) > abs(XM(J)) ) then  ! LAYER ABSORBS ALL
                         MICE(K) = MICE(K) - XM(J)
                         XMF     = XMF + ConstLatHeatFusion * XM(J) / DT
                         STC(K)  = ConstFreezePoint
                         XM(J)   = 0.0
                      else
                         XM(J)   = XM(J) + MLIQ(K)
                         XMF     = XMF - ConstLatHeatFusion * MLIQ(K) / DT
                         MICE(K) = WMASS0(K)
                         STC(K)  = ConstFreezePoint
                      endif
                      MLIQ(K) = max( 0.0, WMASS0(K)-MICE(K) )
                   endif
                enddo
                HEATR(J) = XM(J) * ConstLatHeatFusion / DT
                STC(J)   = ConstFreezePoint + HEATR(J) * FACT(J)
             endif
          enddo
       endif

    endif ! OPT_GLA==1

    !--- update snow and soil ice and liquid content
    do J = ISNOW+1, 0     ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    enddo
    do J = 1, NSOIL       ! glacier ice
       if ( OPT_GLA == 1 ) then
          SH2O(J) = MLIQ(J) / (1000.0 * DZSNSO(J))
          SH2O(J) = max( 0.0, min(1.0,SH2O(J)) )
       elseif ( OPT_GLA == 2 ) then
          SH2O(J) = 0.0             ! ice, assume all frozen forever
       endif
       SMC(J) = 1.0
    enddo

    end associate

  end subroutine GlacierPhaseChange

end module GlacierPhaseChangeMod
