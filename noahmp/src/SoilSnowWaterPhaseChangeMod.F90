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
              OptSoilSupercoolWater => noahmp%config%nmlist%OptSoilSupercoolWater,& ! in,    options for soil supercooled liquid water
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,    maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              IST             => noahmp%config%domain%IST            ,& ! in,    surface type 1-soil; 2-lake
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,    thickness of snow/soil layers (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,    saturated soil matric potential (m)
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,    saturated value of soil moisture [m3/m3]
              FACT            => noahmp%energy%state%FACT            ,& ! in,    energy factor for soil & snow phase change
              STC             => noahmp%energy%state%STC             ,& ! inout, snow and soil layer temperature [K]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout, soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout, total soil moisture [m3/m3]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout, snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout, snow layer liquid water [mm]
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout, snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout, snow water equivalent [mm]
              IMELT           => noahmp%water%state%IMELT            ,& ! out,   phase change index [0-none;1-melt;2-refreeze]
              SUPERCOOL       => noahmp%water%state%SUPERCOOL        ,& ! out,   supercooled water in soil (kg/m2)
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
    QMELT   = 0.0
    PONDING = 0.0
    XMF     = 0.0
    ! supercooled water content
    do J = -NSNOW+1, NSOIL 
         SUPERCOOL(J) = 0.0
    enddo
    ! snow layer water mass
    do J = ISNOW+1, 0
       MICE(J) = SNICE(J)
       MLIQ(J) = SNLIQ(J)
    enddo
    ! soil layer water mass
    do J = 1, NSOIL
       MLIQ(J) = SH2O(J) * DZSNSO(J) * 1000.0
       MICE(J) = (SMC(J) - SH2O(J)) * DZSNSO(J) * 1000.0
    enddo
    ! other required variables
    do J = ISNOW+1, NSOIL
       IMELT(J)  = 0
       HM(J)     = 0.0
       XM(J)     = 0.0
       WICE0(J)  = MICE(J)
       WLIQ0(J)  = MLIQ(J)
       WMASS0(J) = MICE(J) + MLIQ(J)
    enddo

    !--- compute soil supercool water content
    if ( IST == 1 ) then ! land points
       do J = 1, NSOIL
          if ( OptSoilSupercoolWater == 1 ) then
             if ( STC(J) < ConstFreezePoint ) then
                SMP          = ConstLatHeatFusion * (ConstFreezePoint - STC(J)) / (ConstGravityAcc * STC(J)) !(m)
                SUPERCOOL(J) = SMCMAX(J) * (SMP / PSISAT(J)) ** (-1.0 / BEXP(J))
                SUPERCOOL(J) = SUPERCOOL(J) * DZSNSO(J) * 1000.0        !(mm)
             endif
          endif
          if ( OptSoilSupercoolWater == 2 ) then
               call SoilWaterSupercoolLiquid(noahmp, J, SUPERCOOL(J), STC(J), SMC(J), SH2O(J))
               SUPERCOOL(J) = SUPERCOOL(J) * DZSNSO(J) * 1000.0        !(mm)
          endif
       enddo
    endif

    !--- determine melting or freezing state
    do J = ISNOW+1, NSOIL
       if ( (MICE(J) > 0.0) .and. (STC(J) >= ConstFreezePoint) ) then
          IMELT(J) = 1  ! melting
       endif
       if ( (MLIQ(J) > SUPERCOOL(J)) .and. (STC(J) < ConstFreezePoint) ) then
          IMELT(J) = 2  ! freezing
       endif
       ! If snow exists, but its thickness is not enough to create a layer
       if ( (ISNOW == 0) .and. (SNEQV > 0.0) .and. (J == 1) ) then
          if ( STC(J) >= ConstFreezePoint ) then
             IMELT(J) = 1
          endif
       endif
    enddo

    !--- Calculate the energy surplus and loss for melting and freezing
    do J = ISNOW+1, NSOIL
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

    !--- The rate of melting and freezing for snow without a layer, needs more work.
    if ( (ISNOW == 0) .and. (SNEQV > 0.0) .and. (XM(1) > 0.0) ) then
       TEMP1  = SNEQV
       SNEQV  = max( 0.0, TEMP1-XM(1) )
       PROPOR = SNEQV / TEMP1
       SNOWH  = max( 0.0, PROPOR*SNOWH )
       SNOWH  = min( max(SNOWH,SNEQV/500.0), SNEQV/50.0 )  ! limit adjustment to a reasonable density
       HEATR  = HM(1) - ConstLatHeatFusion * (TEMP1 - SNEQV) / DT
       if ( HEATR > 0.0 ) then
          XM(1) = HEATR * DT / ConstLatHeatFusion
          HM(1) = HEATR
       else
          XM(1) = 0.0
          HM(1) = 0.0
       endif
       QMELT   = max( 0.0, (TEMP1-SNEQV) ) / DT
       XMF     = ConstLatHeatFusion * QMELT
       PONDING = TEMP1 - SNEQV
    endif

    ! The rate of melting and freezing for multi-layer snow and soil
    do J = ISNOW+1, NSOIL
       if ( (IMELT(J) > 0) .and. (abs(HM(J)) > 0.0) ) then
          HEATR = 0.0
          if ( XM(J) > 0.0 ) then
             MICE(J) = max( 0.0, WICE0(J)-XM(J) )
             HEATR   = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
          elseif ( XM(J) < 0.0 ) then
             if ( J <= 0 ) then  ! snow layer
                MICE(J) = min( WMASS0(J), WICE0(J)-XM(J) )
             else    ! soil layer
                if ( WMASS0(J) < SUPERCOOL(J) ) then
                   MICE(J) = 0.0
                else
                   MICE(J) = min( WMASS0(J)-SUPERCOOL(J), WICE0(J)-XM(J) )
                   MICE(J) = max( MICE(J), 0.0 )
                endif
             endif
             HEATR = HM(J) - ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
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
                   XM(J+1) = HM(J+1) * DT / ConstLatHeatFusion   ! BARLAGE
                endif
             endif
          endif
          XMF = XMF + ConstLatHeatFusion * (WICE0(J) - MICE(J)) / DT
          ! snow melting rate
          if ( J < 1 ) then
             QMELT = QMELT + max( 0.0, (WICE0(J)-MICE(J)) ) / DT
          endif
       endif
    enddo

    !--- update snow and soil ice and liquid content
    do J = ISNOW+1, 0     ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    enddo
    do J = 1, NSOIL       ! soil
       SH2O(J) = MLIQ(J) / (1000.0 * DZSNSO(J))
       SMC(J)  = (MLIQ(J) + MICE(J)) / (1000.0 * DZSNSO(J))
    enddo

    end associate

  end subroutine SoilSnowWaterPhaseChange

end module SoilSnowWaterPhaseChangeMod
